# =============================================================================
# 06_alerts.R — Journal des alertes (endpoint 6)
# =============================================================================
# IMPORTANT / ASSUMPTION:
# Aucun "registre des alertes et investigations" n'a été fourni (fichier
# mentionné comme nécessaire dans la spec, section "Données de Référence").
# En son absence, les alertes sont **dérivées automatiquement** de
# overall_mape.xlsx : chaque (district, semaine, maladie) dont le nombre de
# cas franchit un seuil (voir DEFAULT_THRESHOLDS dans 00_config.R) génère
# une entrée d'alerte. Les champs qui n'ont pas de source de données réelle
# (fosa, source, statut_traitement, investigation_dans_48h) sont déduits par
# des règles simples documentées ci-dessous. Remplacer cette logique par une
# lecture directe dès qu'un registre d'alertes réel est disponible.
# =============================================================================

library(dplyr)

#' Approxime la date du lundi de la semaine épi (année, semaine).
#' Approximation simple (pas de calcul ISO-8601 strict) suffisante pour
#' l'affichage "JJ/MM/AAAA" attendu par le frontend.
approx_week_date <- function(annee, semaine) {
  as.Date(sprintf("%d-01-01", annee)) + (semaine - 1) * 7
}

#' Construit AlertItemDTO[] pour /api/v1/alerts
build_alerts <- function(annee, semaine_debut, semaine_fin, maladies, regions) {
  period <- resolve_period(semaine_debut, semaine_fin)
  mape <- load_overall_mape()
  region_norms <- normalize_text(regions)
  variables <- unname(DISEASE_MAP[maladies])

  ref <- apply_propagation_rule(build_district_reference())

  candidates <- mape %>%
    filter(annee == !!annee, semaine >= period$start, semaine <= period$end,
           variable %in% variables, region_norm %in% region_norms, cas > 0) %>%
    rowwise() %>%
    mutate(maladie_front = names(DISEASE_MAP)[match(variable, DISEASE_MAP)]) %>%
    ungroup()

  if (nrow(candidates) == 0) return(list())

  candidates <- candidates %>%
    rowwise() %>%
    mutate(.th_alerte = get_thresholds(maladie_front)$alerte,
           .th_epi     = get_thresholds(maladie_front)$epidemique) %>%
    ungroup() %>%
    filter(cas >= .th_alerte) %>%
    arrange(desc(semaine), desc(cas))

  if (nrow(candidates) == 0) return(list())

  candidates <- candidates %>% left_join(
    ref %>% select(district_norm, statut), by = "district_norm"
  )

  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    r <- candidates[i, ]
    dt <- approx_week_date(r$annee, r$semaine)
    seuil_franchi <- if (r$cas >= r$.th_epi) "Seuil \u00c9pid\u00e9mique Franchi" else "Seuil Alerte Franchi"
    statut_traitement <- if (identical(r$statut, "\u00c9pid\u00e9mie")) "Confirm\u00e9e" else
      if (identical(r$statut, "Alerte")) "En cours" else "En attente"
    actions <- if (seuil_franchi == "Seuil \u00c9pid\u00e9mique Franchi")
      "D\u00e9ployer une \u00e9quipe d'investigation rapide sous 48h" else
      "Renforcer la surveillance active et v\u00e9rifier la d\u00e9finition de cas"

    list(
      id_alerte = sprintf("ALT-%d-%04d", r$annee, i),
      code = sprintf("ALRT-CM-%s-%03d", format(dt, "%Y%m%d"), i),
      date_notification = fmt_date(dt),
      maladie = r$maladie_front,
      region = display_region_name(r$region_raw),
      district = display_district_name(r$district_raw),
      fosa = "Non renseign\u00e9",
      cas_notifies = r$cas,
      deces_notifies = r$deces,
      source = "DHIS2",
      statut_seuil = seuil_franchi,
      investigation_dans_48h = seuil_franchi == "Seuil \u00c9pid\u00e9mique Franchi",
      statut_traitement = statut_traitement,
      actions_recommandees = actions
    )
  })

  rows
}
