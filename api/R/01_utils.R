# =============================================================================
# 01_utils.R — Fonctions utilitaires génériques
# =============================================================================

#' Normalise un texte pour servir de clé de jointure (région / district).
#' Enlève accents, préfixes "Region "/"District ", ponctuation, espaces multiples.
normalize_text <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  x <- trimws(x)
  # stringi::stri_trans_general est beaucoup plus fiable qu'iconv(...,
  # "ASCII//TRANSLIT") pour la suppression d'accents (le comportement
  # d'iconv dépend de la locale du système et peut échouer silencieusement,
  # ex: "ê" -> "" au lieu de "e").
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x <- tolower(x)
  x <- gsub("^region\\s+", "", x)
  x <- gsub("^district\\s+", "", x)
  x <- gsub("^district de sante\\s+", "", x)
  x <- gsub("^ds\\s+", "", x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  x
}

#' Extrait le numéro de semaine épidémiologique depuis divers formats
#' rencontrés dans les fichiers sources : "w1", "S24", "S1_2024", "24".
#' Implémenté avec vapply (1 valeur en sortie par élément en entrée) pour
#' éviter les décalages que regmatches() provoque silencieusement en
#' l'absence de correspondance.
parse_week_number <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  x <- as.character(x)
  vapply(x, function(xi) {
    if (is.na(xi)) return(NA_integer_)
    m <- regmatches(xi, regexpr("[0-9]+", xi))
    if (length(m) == 0 || m == "") return(NA_integer_)
    suppressWarnings(as.integer(m))
  }, integer(1), USE.NAMES = FALSE)
}

#' Extrait l'année depuis un format "S1_2024" ou "w1" + colonne year séparée.
#' Retourne NA si absente (dans ce cas utiliser la colonne "year" du fichier).
parse_week_year <- function(x) {
  if (is.null(x) || length(x) == 0) return(integer(0))
  x <- as.character(x)
  vapply(x, function(xi) {
    if (is.na(xi)) return(NA_integer_)
    m <- regmatches(xi, regexpr("(19|20)[0-9]{2}", xi))
    if (length(m) == 0 || m == "") return(NA_integer_)
    suppressWarnings(as.integer(m))
  }, integer(1), USE.NAMES = FALSE)
}

#' Formate un numéro de semaine en étiquette "S24"
epiweek_label <- function(week_num) {
  paste0("S", week_num)
}

#' Découpe un paramètre query string CSV ("Cholera,Mpox") en vecteur.
#' Retourne NULL si vide/absent (= pas de filtre = tout retourner).
csv_param_to_vector <- function(param) {
  if (is.null(param) || is.na(param) || trimws(param) == "") return(NULL)
  parts <- strsplit(param, ",")[[1]]
  trimws(parts)
}

#' Convertit un paramètre query string en entier, avec valeur par défaut.
int_param <- function(param, default = NA_integer_) {
  if (is.null(param) || is.na(param) || trimws(as.character(param)) == "") return(default)
  val <- suppressWarnings(as.integer(param))
  if (is.na(val)) default else val
}

#' Filtre un vecteur de maladies "affichées" (front) à partir du paramètre
#' `maladies` de la query string. Retourne toutes les maladies si vide.
resolve_maladies_filter <- function(maladies_param) {
  requested <- csv_param_to_vector(maladies_param)
  if (is.null(requested)) return(DISEASES_FRONT)
  valid <- intersect(requested, DISEASES_FRONT)
  if (length(valid) == 0) return(DISEASES_FRONT)
  valid
}

#' Filtre un vecteur de régions à partir du paramètre `regions` de la query
#' string. Retourne toutes les régions si vide.
resolve_regions_filter <- function(regions_param) {
  requested <- csv_param_to_vector(regions_param)
  if (is.null(requested)) return(REGIONS_CAMEROUN)
  valid <- intersect(requested, REGIONS_CAMEROUN)
  if (length(valid) == 0) return(REGIONS_CAMEROUN)
  valid
}

#' Formate une date (Date/POSIXct) en "JJ/MM/AAAA"
fmt_date <- function(d) {
  if (is.null(d) || all(is.na(d))) return(NA_character_)
  format(as.Date(d), "%d/%m/%Y")
}

#' Formate une date-heure en "JJ/MM/AAAA HH:MM"
fmt_datetime <- function(dt) {
  if (is.null(dt) || all(is.na(dt))) return(NA_character_)
  format(as.POSIXct(dt), "%d/%m/%Y %H:%M")
}

#' Remplace NA par une valeur par défaut (utile pour cas/deces manquants)
na0 <- function(x, default = 0) {
  x[is.na(x)] <- default
  x
}

#' Calcule la tendance ("hausse"/"baisse"/"stable") entre deux valeurs
compute_tendance <- function(valeur, valeur_precedente, tol = 1e-9) {
  if (is.na(valeur_precedente)) return(NA_character_)
  d <- valeur - valeur_precedente
  if (abs(d) < tol) return("stable")
  if (d > 0) "hausse" else "baisse"
}

#' Variation en % entre deux valeurs (arrondie à 2 décimales)
compute_variation_pct <- function(valeur, valeur_precedente) {
  if (is.na(valeur_precedente) || valeur_precedente == 0) return(NA_real_)
  round((valeur - valeur_precedente) / valeur_precedente * 100, 2)
}

#' round2 : arrondi "propre" évitant les soucis de banker's rounding pour l'affichage
round2 <- function(x, digits = 2) round(x, digits)

# --- Correspondance nom normalisé -> libellé canonique de région -----------
# Les sources de données orthographient les régions différemment
# ("Extreme Nord" vs "Extrême-Nord", "Nord Ouest" vs "Nord-Ouest",
# et certains fichiers utilisent même les noms anglais "North West"/
# "South West"). Cette table ramène toute variante normalisée vers le
# libellé canonique attendu par le frontend (cf. REGIONS_CAMEROUN dans
# 00_config.R).
REGION_NORM_TO_DISPLAY <- setNames(REGIONS_CAMEROUN, normalize_text(REGIONS_CAMEROUN))
REGION_ALIASES <- c(
  "north west" = "Nord-Ouest",
  "south west" = "Sud-Ouest",
  "far north"  = "Extrême-Nord"
)
REGION_NORM_TO_DISPLAY <- c(REGION_NORM_TO_DISPLAY, REGION_ALIASES)

#' Renvoie le libellé canonique d'une région à partir de son nom normalisé.
#' Repli sur une mise en forme "Title Case" si la région est inconnue.
canonical_region_name <- function(region_norm) {
  out <- unname(REGION_NORM_TO_DISPLAY[region_norm])
  ifelse(is.na(out), tools::toTitleCase(region_norm), out)
}
