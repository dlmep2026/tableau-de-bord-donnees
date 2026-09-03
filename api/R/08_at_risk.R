# =============================================================================
# 08_at_risk.R — Priorisation des districts à risque (endpoint 9)
# =============================================================================

library(dplyr)

#' Génère une liste d'actions prioritaires selon le niveau de risque et la
#' contiguïté avec un district en épidémie. Règles simples et documentées ;
#' à affiner avec la DLMEP si un référentiel d'actions existe.
actions_for_district <- function(niveau_risque, est_limitrophe_epidemie, statut) {
  actions <- character(0)
  if (statut == "\u00c9pid\u00e9mie") {
    actions <- c(actions, "Investigation de terrain imm\u00e9diate",
                 "Activation du plan ORSEC", "Renforcement de la surveillance active")
  } else if (est_limitrophe_epidemie) {
    actions <- c(actions, "Vigilance renforc\u00e9e (district limitrophe)",
                 "Pr\u00e9positionnement des intrants")
  } else if (statut == "Alerte") {
    actions <- c(actions, "V\u00e9rification de la d\u00e9finition de cas",
                 "Investigation compl\u00e9mentaire sous 48h")
  } else {
    actions <- c(actions, "Maintien de la surveillance de routine")
  }
  actions
}

#' Construit AtRiskDistrictPrioritizationDTO[] pour /api/v1/districts/at-risk
build_at_risk_districts <- function(annee, semaine_fin, maladies, regions) {
  points <- build_district_map_points(annee, semaine_fin, maladies, regions)

  ranked <- points %>%
    arrange(desc(score_risque)) %>%
    mutate(rang = row_number())

  rows <- lapply(seq_len(nrow(ranked)), function(i) {
    r <- ranked[i, ]
    list(
      rang = r$rang,
      id_district = r$id_district,
      nom_district = r$nom_district,
      region = r$nom_region,
      score_global = r$score_risque,
      niveau_risque = r$niveau_risque,
      est_limitrophe_epidemie = r$est_limitrophe_epidemie,
      districts_en_epidemie_adjacents = I(r$districts_limitrophes_en_epidemie[[1]]),
      actions_prioritaires = I(actions_for_district(r$niveau_risque, r$est_limitrophe_epidemie, r$statut))
    )
  })

  rows
}
