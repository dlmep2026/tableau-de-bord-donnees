# =============================================================================
# 04_case_data.R — Données au niveau cas (linelists) : pyramide âge/sexe
# =============================================================================
# Seuls Cholera et Mpox disposent d'un linelist individuel dans les sources
# fournies. FHV et Syndrome-Grippal n'ont pas de linelist -> la pyramide
# âge/sexe pour ces deux maladies sera vide (tableau de zéros), ce qui est
# documenté dans le README.
# =============================================================================

library(dplyr)

AGE_BREAKS  <- c(-Inf, 4, 14, 29, 44, 59, Inf)
AGE_LABELS  <- c("0\u20134 ans", "5\u201314 ans", "15\u201329 ans",
                  "30\u201344 ans", "45\u201359 ans", "60+ ans")

age_to_tranche <- function(age) {
  cut(age, breaks = AGE_BREAKS, labels = AGE_LABELS, right = TRUE)
}

#' Combine les linelists Mpox + Choléra disponibles en une seule table
#' harmonisée : maladie, region_norm, district_norm, semaine, annee, sexe, age.
build_unified_case_table <- function() {
  cols <- c("maladie", "region_norm", "district_norm", "semaine", "annee",
            "sexe", "age", "is_confirmed", "is_death")
  mpox <- load_ll_mpox() %>% select(all_of(cols))
  chol <- load_ll_cholera() %>% select(all_of(cols))
  bind_rows(mpox, chol)
}

#' Construit la pyramide des âges/sexes pour les filtres donnés.
#' Retourne toujours les 6 tranches d'âge, même à zéro (cf. spec).
build_age_sex_pyramid <- function(annee, semaine_debut, semaine_fin, maladies, regions) {
  cases <- build_unified_case_table()

  region_norms <- normalize_text(regions)

  filtered <- cases %>%
    filter(
      maladie %in% maladies,
      annee == !!annee,
      semaine >= !!semaine_debut, semaine <= !!semaine_fin,
      region_norm %in% region_norms,
      !is.na(age), !is.na(sexe)
    ) %>%
    mutate(tranche_age = age_to_tranche(age))

  summary <- filtered %>%
    filter(!is.na(tranche_age)) %>%
    group_by(tranche_age, sexe) %>%
    summarise(n = n(), .groups = "drop")

  base <- expand.grid(tranche_age = AGE_LABELS, sexe = c("Masculin", "Feminin"), stringsAsFactors = FALSE)
  merged <- base %>%
    left_join(summary, by = c("tranche_age", "sexe")) %>%
    mutate(n = na0(n))

  out <- merged %>%
    tidyr::pivot_wider(names_from = sexe, values_from = n, values_fill = 0) %>%
    transmute(
      tranche_age,
      masculin = na0(Masculin),
      feminin  = na0(Feminin)
    )

  # Réordonne selon AGE_LABELS (pivot_wider peut réordonner alphabétiquement)
  out <- out[match(AGE_LABELS, out$tranche_age), ]
  out
}
