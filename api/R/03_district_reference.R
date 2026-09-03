# =============================================================================
# 03_district_reference.R — Référentiel districts + règle de propagation
# =============================================================================
# Construit la liste maîtresse des districts (à partir de epidemie.xlsx,
# enrichie des coordonnées/population du geojson quand disponibles), calcule
# le statut (Calme/Alerte/Épidémie) et applique la règle de propagation aux
# districts limitrophes d'un district en épidémie.
# =============================================================================

library(dplyr)
library(purrr)

#' Fabrique un identifiant technique stable à partir du nom normalisé
make_id_district <- function(region_norm, district_norm) {
  toupper(paste0("DS_", gsub(" ", "_", district_norm)))
}

#' Met en forme un nom de district pour l'affichage : "DS <Nom>"
display_district_name <- function(district_raw) {
  nm <- gsub("^[Dd]istrict\\s+", "", trimws(district_raw))
  paste("DS", nm)
}

#' Met en forme un nom de région pour l'affichage : renvoie le libellé
#' canonique attendu par le frontend (cf. canonical_region_name / 01_utils.R)
#' plutôt que la graphie brute de la source (qui varie d'un fichier à l'autre).
display_region_name <- function(region_raw) {
  canonical_region_name(normalize_text(region_raw))
}

#' Construit le référentiel de base des districts : un district par ligne,
#' avec statut alerte/épidémie brut (epidemie.xlsx) + géométrie si connue.
build_district_reference <- function() {
  epi <- load_epidemie()
  geo <- load_geo_district()

  ref <- epi %>%
    transmute(
      district_norm, region_norm,
      nom_district = display_district_name(district_raw),
      nom_region   = display_region_name(region_raw),
      is_in_alert, is_in_epidemic
    ) %>%
    distinct(district_norm, .keep_all = TRUE) %>%
    mutate(id_district = make_id_district(region_norm, district_norm))

  if (!is.null(geo) && nrow(geo) > 0) {
    geo_df <- geo %>%
      st_drop_geometry() %>%
      transmute(district_norm, latitude = centroid_lat, longitude = centroid_lon,
                population = suppressWarnings(as.numeric(Pop2024)))
    ref <- ref %>% left_join(geo_df, by = "district_norm")
  } else {
    ref$latitude <- NA_real_
    ref$longitude <- NA_real_
    ref$population <- NA_real_
  }

  # Repli : si un district n'a pas de géométrie propre, utiliser le
  # centroïde de sa région (mieux qu'un point manquant pour l'affichage
  # carte). Population reste NA -> taux d'attaque non calculable pour ces
  # districts (voir compute_taux_attaque()).
  geo_region <- load_geo_region()
  if (!is.null(geo_region) && nrow(geo_region) > 0) {
    reg_cent <- suppressWarnings(st_centroid(st_geometry(st_transform(geo_region, 4326))))
    reg_coords <- st_coordinates(reg_cent)
    reg_lookup <- tibble(
      region_norm = geo_region$region_norm,
      reg_lat = reg_coords[, 2],
      reg_lon = reg_coords[, 1]
    )
    ref <- ref %>%
      left_join(reg_lookup, by = "region_norm") %>%
      mutate(
        latitude  = ifelse(is.na(latitude), reg_lat, latitude),
        longitude = ifelse(is.na(longitude), reg_lon, longitude)
      ) %>%
      select(-reg_lat, -reg_lon)
  }

  ref
}

#' Applique la règle de propagation épidémiologique : tout district
#' géographiquement limitrophe d'un district en "Épidémie" passe au minimum
#' en statut "Alerte", avec `est_limitrophe_epidemie = TRUE` et la liste des
#' voisins en épidémie renseignée.
apply_propagation_rule <- function(district_ref) {
  adjacency <- load_district_adjacency()

  district_ref <- district_ref %>%
    mutate(
      statut = case_when(
        is_in_epidemic == 1 ~ "\u00c9pid\u00e9mie",
        is_in_alert == 1    ~ "Alerte",
        TRUE                 ~ "Calme"
      )
    )

  epidemic_norms <- district_ref$district_norm[district_ref$statut == "\u00c9pid\u00e9mie"]
  name_lookup <- setNames(district_ref$nom_district, district_ref$district_norm)

  get_epidemic_neighbors <- function(dn) {
    neighbors <- adjacency[[dn]]
    if (is.null(neighbors)) return(character(0))
    intersect(neighbors, epidemic_norms)
  }

  district_ref <- district_ref %>%
    rowwise() %>%
    mutate(
      .neighbors_epi = list(get_epidemic_neighbors(district_norm)),
      est_limitrophe_epidemie = length(.neighbors_epi) > 0 && statut != "\u00c9pid\u00e9mie",
      districts_limitrophes_en_epidemie = list(
        if (statut == "\u00c9pid\u00e9mie") character(0) else unname(name_lookup[.neighbors_epi])
      )
    ) %>%
    ungroup() %>%
    mutate(
      statut = ifelse(est_limitrophe_epidemie & statut == "Calme", "Alerte", statut)
    ) %>%
    select(-.neighbors_epi)

  district_ref
}

#' Agrège cas/décès par district à partir de overall_mape.xlsx pour les
#' filtres donnés (maladies affichées, régions, semaine de fin uniquement
#' -> "situation de la semaine", cohérent avec les compteurs de la carte).
aggregate_district_epi <- function(annee, semaine_fin, maladies, regions) {
  mape <- load_overall_mape()
  variables <- unname(DISEASE_MAP[maladies])

  filtered <- mape %>%
    filter(annee == !!annee, semaine == !!semaine_fin, variable %in% variables)

  filtered %>%
    group_by(district_norm) %>%
    summarise(
      total_cas   = sum(cas, na.rm = TRUE),
      total_deces = sum(deces, na.rm = TRUE),
      maladies_actives = list(unique(names(DISEASE_MAP)[match(variable[cas > 0], DISEASE_MAP)])),
      .groups = "drop"
    )
}

#' Calcule le taux d'attaque pour 100 000 habitants. NA si population inconnue.
compute_taux_attaque <- function(total_cas, population) {
  ifelse(is.na(population) | population == 0, NA_real_,
         round2(total_cas / population * 100000, 1))
}

compute_letalite <- function(total_cas, total_deces) {
  ifelse(is.na(total_cas) | total_cas == 0, 0,
         round2(total_deces / total_cas * 100, 2))
}

#' Score de risque composite 0-100 (voir README pour la justification).
#' ASSUMPTION: aucune formule de scoring n'était fournie dans la spec ;
#' cette formule combine statut, taux d'attaque, létalité et contagion
#' de voisinage. A ajuster avec l'équipe épidémiologie si besoin.
compute_score_risque <- function(statut, taux_attaque, letalite_pourcent, est_limitrophe_epidemie) {
  score_statut <- case_when(
    statut == "\u00c9pid\u00e9mie" ~ 40,
    statut == "Alerte"             ~ 20,
    TRUE                            ~ 0
  )
  ta <- ifelse(is.na(taux_attaque), 0, taux_attaque)
  score_attaque <- pmin(ta / 20 * 30, 30)
  score_letalite <- pmin(letalite_pourcent / 20 * 20, 20)
  score_voisinage <- ifelse(est_limitrophe_epidemie, 10, 0)
  round(pmin(score_statut + score_attaque + score_letalite + score_voisinage, 100))
}

niveau_risque_from_score <- function(score) {
  cut(score,
      breaks = c(-Inf, 24, 49, 74, Inf),
      labels = c("Faible", "Mod\u00e9r\u00e9", "\u00c9lev\u00e9", "Tr\u00e8s \u00c9lev\u00e9"),
      right = TRUE) |> as.character()
}

#' Construit la liste complète DistrictMapPointDTO pour les filtres donnés.
#' Réutilisée par les endpoints 3 (carte) et 9 (priorisation).
build_district_map_points <- function(annee, semaine_fin, maladies, regions) {
  ref <- build_district_reference() %>% filter(nom_region %in% regions)
  ref <- apply_propagation_rule(ref)

  epi_agg <- aggregate_district_epi(annee, semaine_fin, maladies, regions)

  out <- ref %>%
    left_join(epi_agg, by = "district_norm") %>%
    rowwise() %>%
    mutate(
      total_cas   = na0(total_cas),
      total_deces = na0(total_deces),
      maladies_actives = list(if (is.null(maladies_actives) || length(maladies_actives) == 0 || all(is.na(maladies_actives))) character(0) else maladies_actives),
      taux_attaque = compute_taux_attaque(total_cas, population),
      letalite_pourcent = compute_letalite(total_cas, total_deces),
      score_risque = compute_score_risque(statut, ifelse(is.na(taux_attaque), 0, taux_attaque), letalite_pourcent, est_limitrophe_epidemie),
      niveau_risque = niveau_risque_from_score(score_risque)
    ) %>%
    ungroup()

  out
}
