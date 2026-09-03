# =============================================================================
# plumber.R — API Backend SIMR (Tableau de bord de Surveillance Épidémiologique)
# MINSANTE / DLMEP / CCOUSP — République du Cameroun
# =============================================================================
# Démarrage : voir start_api.R (pr_run sur le port 8000)
# Toutes les routes sont préfixées /api/v1 (cf. plumber.json ou le montage
# dans start_api.R).
# =============================================================================

source("R/00_config.R")
source("R/01_utils.R")
source("R/02_data_loader.R")
source("R/03_district_reference.R")
source("R/04_case_data.R")
source("R/05_kpis_series.R")
source("R/06_alerts.R")
source("R/07_risk_store.R")
source("R/08_at_risk.R")

library(plumber)

# --- Chargement initial des données au démarrage (peuple le cache) --------
.last_sync_time <- tryCatch(refresh_all_data(), error = function(e) {
  message("ERREUR au chargement initial des données : ", conditionMessage(e))
  Sys.time()
})

#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type")
  if (req$REQUEST_METHOD == "OPTIONS") {
    res$status <- 200
    return(list())
  }
  plumber::forward()
}

#* @filter error_handler
function(req, res) {
  tryCatch({
    plumber::forward()
  }, error = function(e) {
    res$status <- 500
    list(error = TRUE, message = conditionMessage(e))
  })
}

# --- Helpers communs aux handlers (résolution des query params) -----------
resolve_common_params <- function(annee, semaine_debut, semaine_fin, maladies, regions) {
  mape <- load_overall_mape()
  default_annee <- if (nrow(mape) > 0) max(mape$annee, na.rm = TRUE) else as.integer(format(Sys.Date(), "%Y"))
  a <- int_param(annee, default_annee)

  default_fin <- if (nrow(mape) > 0) {
    wk <- mape$semaine[mape$annee == a]
    if (length(wk) > 0) max(wk, na.rm = TRUE) else 1L
  } else 1L
  sf <- int_param(semaine_fin, default_fin)
  sd <- int_param(semaine_debut, NA_integer_)

  list(
    annee = a,
    semaine_debut = sd,
    semaine_fin = sf,
    maladies = resolve_maladies_filter(maladies),
    regions = resolve_regions_filter(regions)
  )
}

# =============================================================================
# ENDPOINT 1 — Statut de synchronisation
# =============================================================================
#* Etat de synchronisation des sources de données
#* @get /api/v1/status/sync
function() {
  file_infos <- file.info(c(PATH_OVERALL_MAPE, PATH_LL_MPOX, PATH_LL_CHOLERA, PATH_EPIDEMIE))
  last_update <- max(file_infos$mtime, na.rm = TRUE)
  mape <- load_overall_mape()
  ll_mpox <- load_ll_mpox()
  ll_cholera <- load_ll_cholera()

  list(
    last_successful_update = fmt_datetime(last_update),
    pipeline_status = "operationnel",
    sources = list(
      dhis2 = list(
        status = "connecte",
        last_sync = fmt_datetime(file.info(PATH_OVERALL_MAPE)$mtime),
        records_count = nrow(mape)
      ),
      ligne_verte_1510 = list(
        status = "connecte",
        last_sync = fmt_datetime(file.info(PATH_LL_CHOLERA)$mtime),
        calls_count = nrow(ll_cholera)
      ),
      surveillance_communautaire = list(
        status = "connecte",
        last_sync = fmt_datetime(file.info(PATH_LL_MPOX)$mtime),
        reports_count = nrow(ll_mpox)
      )
    )
  )
}

# =============================================================================
# ENDPOINT 2 — Indicateurs clés (KPIs)
# =============================================================================
#* KPIs principaux affichés en haut de chaque page
#* @param annee:int
#* @param semaine_debut:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @get /api/v1/indicators
function(annee = NA, semaine_debut = NA, semaine_fin = NA, maladies = NA, regions = NA) {
  p <- resolve_common_params(annee, semaine_debut, semaine_fin, maladies, regions)
  build_indicators(p$annee, p$semaine_debut, p$semaine_fin, p$maladies, p$regions)
}

# =============================================================================
# ENDPOINT 3 — Points cartographiques des districts
# =============================================================================
#* Coordonnées GPS + données épi par district pour la carte Leaflet
#* @param annee:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @get /api/v1/districts/map
function(annee = NA, semaine_fin = NA, maladies = NA, regions = NA) {
  p <- resolve_common_params(annee, NA, semaine_fin, maladies, regions)
  pts <- build_district_map_points(p$annee, p$semaine_fin, p$maladies, p$regions)

  lapply(seq_len(nrow(pts)), function(i) {
    r <- pts[i, ]
    list(
      id_district = r$id_district,
      nom_district = r$nom_district,
      nom_region = r$nom_region,
      latitude = r$latitude,
      longitude = r$longitude,
      statut = r$statut,
      niveau_risque = r$niveau_risque,
      score_risque = r$score_risque,
      total_cas = r$total_cas,
      total_deces = r$total_deces,
      taux_attaque = r$taux_attaque,
      letalite_pourcent = r$letalite_pourcent,
      est_limitrophe_epidemie = r$est_limitrophe_epidemie,
      districts_limitrophes_en_epidemie = I(r$districts_limitrophes_en_epidemie[[1]]),
      maladies_actives = I(r$maladies_actives[[1]])
    )
  })
}

# =============================================================================
# ENDPOINT 4 — Séries temporelles (courbe épidémique)
# =============================================================================
#* Séries hebdomadaires cas suspects / confirmés / décès
#* @param annee:int
#* @param semaine_debut:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @get /api/v1/epidemiology/series
function(annee = NA, semaine_debut = NA, semaine_fin = NA, maladies = NA, regions = NA) {
  p <- resolve_common_params(annee, semaine_debut, semaine_fin, maladies, regions)
  s <- build_series(p$annee, p$semaine_debut, p$semaine_fin, p$maladies, p$regions)
  lapply(seq_len(nrow(s)), function(i) as.list(s[i, ]))
}

# =============================================================================
# ENDPOINT 5 — Ventilations par maladie et par région
# =============================================================================
#* Répartition des cas par maladie et par région
#* @param annee:int
#* @param semaine_debut:int
#* @param semaine_fin:int
#* @param regions
#* @get /api/v1/epidemiology/breakdowns
function(annee = NA, semaine_debut = NA, semaine_fin = NA, regions = NA) {
  p <- resolve_common_params(annee, semaine_debut, semaine_fin, NA, regions)
  build_breakdowns(p$annee, p$semaine_debut, p$semaine_fin, p$regions)
}

# =============================================================================
# ENDPOINT 6 — Journal des alertes
# =============================================================================
#* Alertes récentes (dérivées des seuils, voir R/06_alerts.R)
#* @param annee:int
#* @param semaine_debut:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @param limit:int Nombre maximum d'alertes retournées (défaut 100)
#* @get /api/v1/alerts
function(annee = NA, semaine_debut = NA, semaine_fin = NA, maladies = NA, regions = NA, limit = NA) {
  p <- resolve_common_params(annee, semaine_debut, semaine_fin, maladies, regions)
  lim <- int_param(limit, 100)
  build_alerts(p$annee, p$semaine_debut, p$semaine_fin, p$maladies, p$regions, limit = lim)
}

# =============================================================================
# ENDPOINT 7 — Pyramide des âges et des sexes
# =============================================================================
#* Pyramide âges/sexes (Cholera et Mpox uniquement, voir R/04_case_data.R)
#* @param annee:int
#* @param semaine_debut:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @get /api/v1/epidemiology/age-sex-pyramid
function(annee = NA, semaine_debut = NA, semaine_fin = NA, maladies = NA, regions = NA) {
  p <- resolve_common_params(annee, semaine_debut, semaine_fin, maladies, regions)
  wd <- if (is.na(p$semaine_debut)) 1L else p$semaine_debut
  pyr <- build_age_sex_pyramid(p$annee, wd, p$semaine_fin, p$maladies, p$regions)
  lapply(seq_len(nrow(pyr)), function(i) as.list(pyr[i, ]))
}

# =============================================================================
# ENDPOINT 8 — Historique des évaluations de risque (GET/POST)
# =============================================================================
#* Liste des évaluations de risque enregistrées
#* @get /api/v1/risk-evaluations
function() {
  read_risk_evaluations()
}

#* Enregistre une nouvelle évaluation de risque (matrice 5x5)
#* @post /api/v1/risk-evaluations
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
                    error = function(e) NULL)
  if (is.null(body)) {
    res$status <- 400
    return(list(error = TRUE, message = "Corps de requ\u00eate JSON invalide"))
  }
  add_risk_evaluation(body)
}

# =============================================================================
# ENDPOINT 9 — Districts à risque (priorisation)
# =============================================================================
#* Classement des districts par niveau de risque
#* @param annee:int
#* @param semaine_fin:int
#* @param maladies
#* @param regions
#* @get /api/v1/districts/at-risk
function(annee = NA, semaine_fin = NA, maladies = NA, regions = NA) {
  p <- resolve_common_params(annee, NA, semaine_fin, maladies, regions)
  build_at_risk_districts(p$annee, p$semaine_fin, p$maladies, p$regions)
}

# =============================================================================
# ENDPOINT 10 — Résumé par maladie (page Accueil)
# =============================================================================
#* Synthèse des 4 maladies prioritaires pour la page d'Accueil
#* @param annee:int
#* @param semaine_fin:int
#* @get /api/v1/home/disease-summary
function(annee = NA, semaine_fin = NA) {
  p <- resolve_common_params(annee, NA, semaine_fin, NA, NA)
  build_disease_summary(p$annee, p$semaine_debut, p$semaine_fin)
}
