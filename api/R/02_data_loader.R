# =============================================================================
# 02_data_loader.R — Chargement et normalisation des sources de données brutes
# =============================================================================
# Chaque fonction load_xxx() lit un fichier source et retourne un data.frame
# normalisé (colonnes *_norm pour les jointures). Les résultats sont mis en
# cache en mémoire (.simr_cache) pour éviter de relire les fichiers à chaque
# requête HTTP. Utiliser refresh_all_data() pour forcer un rechargement
# (c'est ce que fait GET /api/v1/status/sync indirectement).
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(sf)

# Les géométries sources (geojson) contiennent des topologies imparfaites
# (sommets dupliqués, arêtes qui se croisent) que le moteur sphérique s2
# rejette strictement. Le moteur GEOS planaire (legacy) est plus tolérant et
# suffisant pour des calculs de centroïde/adjacence à l'échelle d'un pays.
sf::sf_use_s2(FALSE)

.simr_cache <- new.env(parent = emptyenv())

#' Charge overall_mape.xlsx : table longue hebdomadaire
#' (region, district, variable/maladie, semaine, annee, cas, deces,
#'  expected_reports, actual_reports, received_on_time)
load_overall_mape <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$overall_mape)) return(.simr_cache$overall_mape)

  df <- read_excel(PATH_OVERALL_MAPE, sheet = 1)
  df <- df %>%
    rename(
      region_raw   = orgunitlevel2,
      district_raw = organisationunitname,
      variable     = variable,
      week_raw     = week,
      annee        = year
    ) %>%
    mutate(
      region_norm   = if ("region_norm" %in% names(.)) region_norm else normalize_text(region_raw),
      district_norm = if ("name_norm" %in% names(.)) name_norm else normalize_text(district_raw),
      semaine       = parse_week_number(week_raw),
      cas           = na0(as.numeric(cas)),
      deces         = na0(as.numeric(deces)),
      expected_reports  = na0(as.numeric(expected_reports)),
      actual_reports    = na0(as.numeric(actual_reports)),
      received_on_time  = na0(as.numeric(received_on_time))
    ) %>%
    select(region_raw, district_raw, region_norm, district_norm, variable,
           semaine, annee, cas, deces, expected_reports, actual_reports, received_on_time)

  .simr_cache$overall_mape <- df
  df
}

#' Charge epidemie.xlsx : statut alerte/épidémie déclaré par district
load_epidemie <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$epidemie)) return(.simr_cache$epidemie)

  df <- read_excel(PATH_EPIDEMIE, sheet = 1) %>%
    mutate(
      region_norm   = normalize_text(region),
      district_norm = normalize_text(district),
      is_in_alert    = as.integer(na0(as.numeric(is_in_alert))),
      is_in_epidemic = as.integer(na0(as.numeric(is_in_epidemic)))
    ) %>%
    rename(region_raw = region, district_raw = district) %>%
    select(region_raw, district_raw, region_norm, district_norm, is_in_alert, is_in_epidemic)

  .simr_cache$epidemie <- df
  df
}

#' Charge le geojson des régions (10 polygones)
load_geo_region <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$geo_region)) return(.simr_cache$geo_region)

  geo <- tryCatch(st_read(PATH_REGION_GEOJSON, quiet = TRUE), error = function(e) NULL)
  if (!is.null(geo)) {
    # Certaines géométries sources contiennent des sommets dupliqués /
    # auto-intersections invalides pour le moteur s2 -> réparation.
    geo <- tryCatch(st_make_valid(geo), error = function(e) geo)
    if (!"region_norm" %in% names(geo)) geo$region_norm <- normalize_text(geo$Nom_Region)
  }
  .simr_cache$geo_region <- geo
  geo
}

#' Charge le geojson des districts (peut être partiel : voir README)
load_geo_district <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$geo_district)) return(.simr_cache$geo_district)

  geo <- tryCatch(st_read(PATH_DISTRICT_GEOJSON, quiet = TRUE), error = function(e) NULL)
  if (!is.null(geo)) {
    geo <- tryCatch(st_make_valid(geo), error = function(e) geo)
    geo$district_norm <- normalize_text(geo$District_S)
    geo$region_norm    <- normalize_text(geo$Nom_Region)
    # centroïdes en lat/lon (WGS84) pour la carte Leaflet
    geo <- st_transform(geo, 4326)
    cent <- suppressWarnings(st_centroid(st_geometry(geo)))
    coords <- st_coordinates(cent)
    geo$centroid_lon <- coords[, 1]
    geo$centroid_lat <- coords[, 2]
  }
  .simr_cache$geo_district <- geo
  geo
}

#' Charge la table d'adjacence des districts, calculée depuis le geojson
#' (deux districts sont "limitrophes" si leurs polygones se touchent).
#' NOTE: si district.geojson est partiel (ex: une seule région), l'adjacence
#' n'est calculable que pour les districts présents dans ce fichier.
load_district_adjacency <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$adjacency)) return(.simr_cache$adjacency)

  geo <- load_geo_district(force = force)
  if (is.null(geo) || nrow(geo) == 0) {
    .simr_cache$adjacency <- list()
    return(list())
  }

  touch_mat <- suppressWarnings(st_touches(geo))
  adj <- setNames(vector("list", nrow(geo)), geo$district_norm)
  for (i in seq_len(nrow(geo))) {
    neighbor_idx <- touch_mat[[i]]
    adj[[i]] <- geo$district_norm[neighbor_idx]
  }
  .simr_cache$adjacency <- adj
  adj
}

#' Charge le linelist Mpox (LL_MPOX.xlsx) et retourne un tableau minimal
#' harmonisé : maladie, region_norm, district_norm, semaine, annee, sexe,
#' age, is_confirmed, is_death, date_notification.
load_ll_mpox <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$ll_mpox)) return(.simr_cache$ll_mpox)

  raw <- read_excel(PATH_LL_MPOX, sheet = "LL_MPOX")

  region_col   <- if ("Region de notification" %in% names(raw)) "Region de notification" else "Region d'origine"
  district_col <- if ("District de notification" %in% names(raw)) "District de notification" else "DS d'origine"

  df <- raw %>%
    transmute(
      maladie       = "Mpox",
      region_norm   = normalize_text(.data[[region_col]]),
      district_norm = normalize_text(.data[[district_col]]),
      semaine       = parse_week_number(.data[["Epi_week date notification niveau central"]]),
      annee         = parse_week_year(.data[["Epi_week date notification niveau central"]]),
      date_notification = as.Date(.data[["Date de notification niveau central"]]),
      sexe          = .data[["Sexe"]],
      age           = suppressWarnings(as.numeric(.data[["Age"]])),
      classification = toupper(trimws(as.character(.data[["Classification finale"]]))),
      statut_vital  = toupper(trimws(as.character(.data[["Statut du cas (vivant, decede)"]])))
    ) %>%
    mutate(
      # ASSUMPTION: un cas est "confirmé" si Classification finale == "CONFIRME".
      # Toute autre ligne notifiée (hors "NON-CAS") est comptée en "suspect".
      is_non_cas = !is.na(classification) & classification == "NON-CAS",
      is_confirmed = !is.na(classification) & classification == "CONFIRME",
      is_death    = !is.na(statut_vital) & statut_vital %in% c("DECEDE", "DÉCÉDÉ", "DCD")
    ) %>%
    filter(!is_non_cas)

  .simr_cache$ll_mpox <- df
  df
}

#' Charge le linelist Choléra (LL_CHOLERA_NATIONAL.xlsx) et retourne un
#' tableau minimal harmonisé (mêmes colonnes que load_ll_mpox()).
load_ll_cholera <- function(force = FALSE) {
  if (!force && !is.null(.simr_cache$ll_cholera)) return(.simr_cache$ll_cholera)

  raw <- read_excel(PATH_LL_CHOLERA, sheet = 1)

  region_col   <- "Region"
  district_col <- if ("Health district notifying" %in% names(raw)) "Health district notifying" else "DS_origine"

  df <- raw %>%
    transmute(
      maladie       = "Cholera",
      region_norm   = normalize_text(.data[[region_col]]),
      district_norm = normalize_text(.data[[district_col]]),
      semaine       = parse_week_number(.data[["Epiweek date notification"]]),
      annee         = parse_week_year(.data[["Epiweek date notification"]]),
      date_notification = as.Date(.data[["Date of notification"]]),
      sexe          = .data[["Sex"]],
      age           = suppressWarnings(as.numeric(.data[["Age (year)"]])),
      rdt           = toupper(trimws(as.character(.data[["Result of RDT"]]))),
      culture       = toupper(trimws(as.character(.data[["Result of culture"]]))),
      outcome       = suppressWarnings(as.numeric(.data[["Outcome (2=Healed, 3=Dead)"]]))
    ) %>%
    mutate(
      sexe = case_when(
        toupper(trimws(sexe)) %in% c("F", "FEMININ", "FEMME") ~ "Feminin",
        toupper(trimws(sexe)) %in% c("M", "MASCULIN", "HOMME") ~ "Masculin",
        TRUE ~ NA_character_
      ),
      # ASSUMPTION: cas "confirmé" = RDT positif OU culture positive.
      # Tout cas notifié respectant la définition de cas suspect de choléra
      # (chaque ligne du linelist) est compté en "suspect".
      is_confirmed = (!is.na(rdt) & str_detect(rdt, "POS")) |
                     (!is.na(culture) & str_detect(culture, "POS")),
      is_death = !is.na(outcome) & outcome == 3
    )

  .simr_cache$ll_cholera <- df
  df
}

#' Recharge toutes les sources de données (force = TRUE) et renvoie
#' l'heure de la dernière modification de chaque fichier sur disque
#' (utilisé par /api/v1/status/sync comme proxy de "dernière synchro").
refresh_all_data <- function() {
  load_overall_mape(force = TRUE)
  load_epidemie(force = TRUE)
  load_geo_region(force = TRUE)
  load_geo_district(force = TRUE)
  load_district_adjacency(force = TRUE)
  load_ll_mpox(force = TRUE)
  load_ll_cholera(force = TRUE)

  paths <- c(PATH_OVERALL_MAPE, PATH_LL_MPOX, PATH_LL_CHOLERA,
             PATH_EPIDEMIE, PATH_REGION_GEOJSON, PATH_DISTRICT_GEOJSON)
  mtimes <- file.info(paths)$mtime
  max(mtimes, na.rm = TRUE)
}
