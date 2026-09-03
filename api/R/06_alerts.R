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
#'
#' @param limit Nombre maximum d'alertes retournées (les plus récentes en
#'   premier). Défaut 100 -- un flux d'alertes n'a pas vocation à renvoyer
#'   un historique complet d'année, et ça évite un temps de réponse élevé
#'   quand de nombreux seuils sont franchis sur une longue période.
build_alerts <- function(annee, semaine_debut, semaine_fin, maladies, regions, limit = 100) {
  # ASSUMPTION: si semaine_debut n'est pas fourni, on limite par défaut aux
  # 8 dernières semaines (plutôt qu'au cumul depuis le début de l'année,
  # comme pour /indicators) -- un flux d'alertes doit rester "réactif",
  # pas remonter tout l'historique de l'année par défaut. Le paramètre
  # semaine_debut reste disponible pour consulter une période plus large.
  wd <- if (is.na(semaine_debut)) max(1L, semaine_fin - 7L) else semaine_debut
  period <- list(start = wd, end = semaine_fin)

  mape <- load_overall_mape()
  region_norms <- normalize_text(regions)
  variables <- unname(DISEASE_MAP[maladies])

  ref <- apply_propagation_rule(build_district_reference())

  # Table de seuils vectorisée (maladie -> alerte/épidémique), jointe une
  # seule fois plutôt que recalculée ligne par ligne (rowwise() sur des
  # milliers de lignes était la principale cause de lenteur de l'endpoint).
  th_lookup <- tibble(
    maladie_front = names(DISEASE_MAP),
    variable = unname(DISEASE_MAP)
  ) %>%
    rowwise() %>%
    mutate(th_alerte = get_thresholds(maladie_front)$alerte,
           th_epi     = get_thresholds(maladie_front)$epidemique) %>%
    ungroup()

  candidates <- mape %>%
    filter(annee == !!annee, semaine >= period$start, semaine <= period$end,
           variable %in% variables, region_norm %in% region_norms, cas > 0) %>%
    left_join(th_lookup, by = "variable") %>%
    filter(cas >= th_alerte) %>%
    arrange(desc(semaine), desc(cas))

  if (nrow(candidates) == 0) return(list())

  # Le classement/limitation se fait AVANT la construction des objets de
  # sortie (boucle lapply ci-dessous) : inutile de construire des milliers
  # d'objets pour n'en garder que les `limit` premiers.
  candidates <- candidates %>% left_join(
    ref %>% select(district_norm, statut), by = "district_norm"
  )
  candidates <- head(candidates, limit)

  rows <- lapply(seq_len(nrow(candidates)), function(i) {
    r <- candidates[i, ]
    dt <- approx_week_date(r$annee, r$semaine)
    seuil_franchi <- if (r$cas >= r$th_epi) "Seuil \u00c9pid\u00e9mique Franchi" else "Seuil Alerte Franchi"
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
