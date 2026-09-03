# =============================================================================
# 05_kpis_series.R — KPIs, séries temporelles, ventilations, résumé maladies
# =============================================================================
# ASSUMPTION (comparaison "période précédente"):
# `valeur_precedente` est calculée sur une fenêtre de même largeur que la
# période sélectionnée (semaine_debut..semaine_fin), immédiatement
# précédente. Ex: si la période sélectionnée est S30-S35 (6 semaines), la
# période précédente est S24-S29. Si semaine_debut n'est pas fourni, la
# période sélectionnée = [1, semaine_fin] (cumul depuis le début de l'année
# épidémiologique) et la période précédente = [1, semaine_debut-1].
# =============================================================================

library(dplyr)

resolve_period <- function(semaine_debut, semaine_fin) {
  wd <- if (is.na(semaine_debut)) 1L else semaine_debut
  list(start = wd, end = semaine_fin)
}

previous_period <- function(period) {
  width <- period$end - period$start + 1
  prev_end <- period$start - 1
  prev_start <- prev_end - width + 1
  if (prev_end < 1) return(NULL)
  list(start = max(prev_start, 1), end = prev_end)
}

#' Complétude & promptitude nationales (%) sur une fenêtre de semaines.
#' Dédoublonne par (district, semaine) car expected/actual_reports sont
#' répétés à l'identique pour chaque "variable" dans overall_mape.xlsx.
compute_completude_promptitude <- function(annee, period, regions) {
  if (is.null(period)) return(list(completude = NA_real_, promptitude = NA_real_))
  mape <- load_overall_mape()
  region_norms <- normalize_text(regions)

  reports <- mape %>%
    filter(annee == !!annee, semaine >= period$start, semaine <= period$end,
           region_norm %in% region_norms) %>%
    distinct(district_norm, semaine, .keep_all = TRUE)

  if (nrow(reports) == 0) return(list(completude = NA_real_, promptitude = NA_real_))

  completude  <- sum(reports$actual_reports) / sum(reports$expected_reports) * 100
  # Si la colonne received_on_time est absente de la source (voir
  # load_overall_mape()), toutes les valeurs sont NA -> promptitude
  # indisponible (NA / null en JSON), plutôt qu'un 0% trompeur.
  promptitude <- if (all(is.na(reports$received_on_time))) {
    NA_real_
  } else {
    sum(reports$received_on_time, na.rm = TRUE) / pmax(sum(reports$expected_reports), 1) * 100
  }

  list(completude = round2(completude, 2), promptitude = round2(promptitude, 2))
}

#' Total cas / décès pour les maladies+régions sélectionnées sur une fenêtre.
compute_total_cas_deces <- function(annee, period, maladies, regions) {
  if (is.null(period)) return(list(cas = 0, deces = 0))
  mape <- load_overall_mape()
  variables <- unname(DISEASE_MAP[maladies])
  region_norms <- normalize_text(regions)

  filtered <- mape %>%
    filter(annee == !!annee, semaine >= period$start, semaine <= period$end,
           variable %in% variables, region_norm %in% region_norms)

  list(cas = sum(filtered$cas), deces = sum(filtered$deces))
}

#' Construit le tableau IndicatorDTO[] pour /api/v1/indicators
build_indicators <- function(annee, semaine_debut, semaine_fin, maladies, regions) {
  period <- resolve_period(semaine_debut, semaine_fin)
  prev   <- previous_period(period)

  cp_now  <- compute_completude_promptitude(annee, period, regions)
  cp_prev <- if (is.null(prev)) list(completude = NA_real_, promptitude = NA_real_) else
    compute_completude_promptitude(annee, prev, regions)

  cd_now  <- compute_total_cas_deces(annee, period, maladies, regions)
  cd_prev <- if (is.null(prev)) list(cas = NA_real_, deces = NA_real_) else
    compute_total_cas_deces(annee, prev, maladies, regions)

  letalite <- if (cd_now$cas == 0) 0 else round2(cd_now$deces / cd_now$cas * 100, 2)

  ref <- apply_propagation_rule(build_district_reference() %>% filter(nom_region %in% regions))
  districts_en_epidemie <- sum(ref$statut == "\u00c9pid\u00e9mie")
  districts_en_alerte   <- sum(ref$statut == "Alerte")
  districts_frontaliers_risque <- sum(ref$est_limitrophe_epidemie)
  cumul_alertes <- sum(ref$is_in_alert == 1 | ref$is_in_epidemic == 1)

  mk <- function(id, libelle, valeur, unite = NULL, valeur_prec = NA_real_,
                 est_hausse_critique = NULL) {
    tendance <- if (!is.na(valeur_prec)) compute_tendance(valeur, valeur_prec) else NA_character_
    variation <- if (!is.na(valeur_prec)) compute_variation_pct(valeur, valeur_prec) else NA_real_
    item <- list(id = id, libelle = libelle, valeur = valeur)
    if (!is.null(unite)) item$unite <- unite
    if (!is.na(valeur_prec)) item$valeur_precedente <- valeur_prec
    if (!is.na(variation)) item$variation_pourcentage <- variation
    if (!is.na(tendance)) item$tendance <- tendance
    if (!is.null(est_hausse_critique)) item$est_hausse_critique <- est_hausse_critique
    item
  }

  list(
    mk("completude_nationale", "Compl\u00e9tude nationale", cp_now$completude, "%", cp_prev$completude, FALSE),
    mk("promptitude_nationale", "Promptitude nationale", cp_now$promptitude, "%", cp_prev$promptitude, FALSE),
    mk("total_cas", "Total des cas", cd_now$cas, "cas", cd_prev$cas, TRUE),
    mk("total_deces", "Total des d\u00e9c\u00e8s", cd_now$deces, "d\u00e9c\u00e8s", cd_prev$deces, TRUE),
    mk("letalite_cfr", "L\u00e9talit\u00e9 (CFR)", letalite, "%"),
    mk("districts_en_epidemie", "Districts en \u00e9pid\u00e9mie", districts_en_epidemie),
    mk("districts_frontaliers_risque", "Districts frontaliers \u00e0 risque", districts_frontaliers_risque),
    mk("cumul_alertes", "Cumul des alertes", cumul_alertes),
    mk("districts_en_alerte", "Districts en alerte", districts_en_alerte)
  )
}

#' Construit la courbe épidémique SeriesPointDTO[] pour /epidemiology/series
build_series <- function(annee, semaine_debut, semaine_fin, maladies, regions) {
  mape <- load_overall_mape()
  cases <- build_unified_case_table()
  variables <- unname(DISEASE_MAP[maladies])
  region_norms <- normalize_text(regions)
  wd <- if (is.na(semaine_debut)) 1L else semaine_debut

  weeks <- seq.int(wd, semaine_fin)

  # cas confirmés/décès depuis overall_mape (comptage agrégé fiable)
  agg <- mape %>%
    filter(annee == !!annee, semaine %in% weeks, variable %in% variables,
           region_norm %in% region_norms) %>%
    group_by(semaine) %>%
    summarise(cas_total = sum(cas), deces = sum(deces), .groups = "drop")

  # part "confirmée" depuis les linelists disponibles (Cholera, Mpox)
  conf <- cases %>%
    filter(maladie %in% maladies, annee == !!annee, semaine %in% weeks,
           region_norm %in% region_norms) %>%
    group_by(semaine) %>%
    summarise(cas_confirmes = sum(is_confirmed, na.rm = TRUE), .groups = "drop")

  th <- get_thresholds(maladies[1])

  out <- tibble(semaine = weeks) %>%
    left_join(agg, by = "semaine") %>%
    left_join(conf, by = "semaine") %>%
    mutate(
      cas_total = na0(cas_total),
      deces = na0(deces),
      cas_confirmes = pmin(na0(cas_confirmes), cas_total)
    ) %>%
    transmute(
      periode = epiweek_label(semaine),
      cas_suspects = cas_total,
      cas_confirmes,
      deces,
      seuil_alerte = th$alerte,
      seuil_epidemique = th$epidemique
    )

  out
}

#' Construit les ventilations par maladie et par région pour /breakdowns
build_breakdowns <- function(annee, semaine_debut, semaine_fin, regions) {
  period <- resolve_period(semaine_debut, semaine_fin)
  mape <- load_overall_mape()
  region_norms <- normalize_text(regions)

  base <- mape %>%
    filter(annee == !!annee, semaine >= period$start, semaine <= period$end,
           region_norm %in% region_norms)

  by_disease <- lapply(names(DISEASE_MAP), function(mal) {
    var <- DISEASE_MAP[[mal]]
    sub <- base %>% filter(variable == var)
    cas <- sum(sub$cas); deces <- sum(sub$deces)
    list(
      maladie = mal,
      cas = cas,
      deces = deces,
      letalite_pourcent = if (cas == 0) 0 else round2(deces / cas * 100, 2),
      districts_touches = length(unique(sub$district_norm[sub$cas > 0]))
    )
  })

  ref <- apply_propagation_rule(build_district_reference())

  by_region <- lapply(regions, function(reg) {
    reg_norm <- normalize_text(reg)
    sub <- base %>% filter(region_norm == reg_norm, variable %in% unname(DISEASE_MAP))
    reg_districts <- ref %>% filter(region_norm == reg_norm)
    list(
      region = reg,
      cas = sum(sub$cas),
      deces = sum(sub$deces),
      districts_en_epidemie = sum(reg_districts$statut == "\u00c9pid\u00e9mie"),
      districts_en_alerte   = sum(reg_districts$statut == "Alerte")
    )
  })

  list(byDisease = by_disease, byRegion = by_region)
}

#' Construit le résumé par maladie pour la page d'Accueil (endpoint 10)
build_disease_summary <- function(annee, semaine_debut, semaine_fin) {
  period <- resolve_period(semaine_debut, semaine_fin)
  mape <- load_overall_mape()
  ref <- apply_propagation_rule(build_district_reference())

  lapply(names(DISEASE_MAP), function(mal) {
    var <- DISEASE_MAP[[mal]]

    week_now <- mape %>% filter(annee == !!annee, semaine == !!semaine_fin, variable == var)
    nvx_cas_semaine <- sum(week_now$cas)

    districts_maladie <- unique(week_now$district_norm[week_now$cas > 0])
    ds_ref <- ref %>% filter(district_norm %in% districts_maladie)
    ds_en_epidemie <- sum(ds_ref$statut == "\u00c9pid\u00e9mie")
    ds_en_alerte   <- sum(ds_ref$statut == "Alerte")

    statut <- if (ds_en_epidemie > 0) "Critique" else if (ds_en_alerte > 0) "Sous surveillance" else "Stable"

    list(
      maladie = mal,
      statut = statut,
      ds_en_epidemie = ds_en_epidemie,
      ds_en_alerte = ds_en_alerte,
      nvx_cas_semaine = nvx_cas_semaine
    )
  })
}
