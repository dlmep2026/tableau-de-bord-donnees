# =============================================================================
# 00_config.R — Configuration centrale du backend SIMR
# =============================================================================
# Chemins, constantes métier et table de correspondance des maladies.
# Toute constante partagée entre les fichiers R/*.R doit vivre ici.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- Chemins des fichiers de données --------------------------------------
# NOTE: chemins relatifs à la racine du projet (là où se trouve plumber.R).
# Lancer l'API depuis cette racine (voir start_api.R), ou adapter DATA_DIR
# ci-dessous si le projet est déplacé.
DATA_DIR <- "data"

PATH_OVERALL_MAPE   <- file.path(DATA_DIR, "overall_mape.xlsx")
PATH_LL_MPOX        <- file.path(DATA_DIR, "LL_MPOX.xlsx")
PATH_LL_CHOLERA     <- file.path(DATA_DIR, "LL_CHOLERA_NATIONAL.xlsx")
PATH_EPIDEMIE       <- file.path(DATA_DIR, "epidemie.xlsx")
PATH_REGION_GEOJSON <- file.path(DATA_DIR, "region.geojson")
PATH_DISTRICT_GEOJSON <- file.path(DATA_DIR, "district.geojson")
PATH_RISK_EVAL_STORE <- file.path(DATA_DIR, "risk_evaluations_store.json")

# --- Récupération des données depuis GitHub (optionnel) --------------------
# Si USE_GITHUB_SOURCES est TRUE, chaque fichier ci-dessous est téléchargé
# depuis son URL "raw" GitHub vers data/ avant le premier chargement (voir
# download_github_sources() dans R/02_data_loader.R). Remplacez les URLs
# <owner>/<repo>/<branch>/<path> par les vraies URLs "raw" de votre dépôt
# (bouton "Raw" sur la page du fichier dans GitHub, ou clic droit > copier
# le lien du bouton "Raw").
USE_GITHUB_SOURCES <- TRUE

GITHUB_SOURCES <- list(
  overall_mape      = "https://raw.githubusercontent.com/dlmep2026/tableau-de-bord-donnees/main/overall_mape.xlsx",
  ll_mpox           = "https://raw.githubusercontent.com/<owner>/<repo>/<branch>/<path>/LL_MPOX.xlsx",
  ll_cholera        = "https://raw.githubusercontent.com/<owner>/<repo>/<branch>/<path>/LL_CHOLERA_NATIONAL.xlsx",
  epidemie          = "https://raw.githubusercontent.com/<owner>/<repo>/<branch>/<path>/epidemie.xlsx",
  region_geojson    = "https://raw.githubusercontent.com/dlmep2026/tableau-de-bord-donnees/main/region.geojson",
  district_geojson  = "https://raw.githubusercontent.com/dlmep2026/tableau-de-bord-donnees/main/district_sante_206.geojson"
)

# Si le dépôt est PRIVÉ, renseignez un Personal Access Token GitHub dans la
# variable d'environnement GITHUB_PAT (ex: export GITHUB_PAT=ghp_xxx avant
# de lancer Rscript, ou dans un fichier .Renviron). Laissez vide si public.
GITHUB_PAT <- Sys.getenv("GITHUB_PAT", unset = "")

# --- Les 4 maladies prioritaires (obligatoire, cf. spec) --------------------
# Nom affiché (frontend)  <->  code "variable" dans overall_mape.xlsx
DISEASE_MAP <- c(
  "Cholera"          = "cholera",
  "Mpox"             = "variole_du_singe_monkey_pox",
  "FHV"              = "fhv",
  "Syndrome-Grippal" = "syndrome_grippal"
)
DISEASES_FRONT <- names(DISEASE_MAP)

# Alias reconnus lorsqu'on lit la colonne "Region"/"maladie" des linelists
# (le linelist mpox n'a pas de colonne "maladie" -> tout le fichier = Mpox)
LINELIST_DISEASE <- list(
  Cholera = "Cholera",
  Mpox    = "Mpox"
)

# --- Les 10 régions du Cameroun (obligatoire, cf. spec) ---------------------
REGIONS_CAMEROUN <- c(
  "Adamaoua", "Centre", "Est", "Extrême-Nord", "Littoral",
  "Nord", "Nord-Ouest", "Ouest", "Sud", "Sud-Ouest"
)


# --- Seuils d'alerte / épidémiques par défaut (à défaut de fichier dédié) ---
# NOTE: aucun fichier "seuils d'alerte et épidémiques par maladie et district"
# n'a été fourni. On applique des seuils par défaut, par maladie, au niveau
# district et par semaine. A remplacer facilement si un fichier de seuils
# est fourni ultérieurement (voir get_thresholds()).
DEFAULT_THRESHOLDS <- list(
  "Cholera"          = list(alerte = 2,  epidemique = 5),
  "Mpox"             = list(alerte = 1,  epidemique = 3),
  "FHV"              = list(alerte = 1,  epidemique = 2),
  "Syndrome-Grippal"  = list(alerte = 10, epidemique = 20)
)

get_thresholds <- function(maladie) {
  th <- DEFAULT_THRESHOLDS[[maladie]]
  if (is.null(th)) th <- list(alerte = 5, epidemique = 10)
  th
}

# --- Sources déclarées dans /api/v1/status/sync ------------------------------
# NOTE: pas de connexion temps réel à DHIS2 / Ligne Verte 1510 / surveillance
# communautaire disponible ici -> on reporte l'heure de dernière modification
# des fichiers sources locaux comme proxy de "dernière synchronisation".
