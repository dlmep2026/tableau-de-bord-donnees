# =============================================================================
# 07_risk_store.R — Persistance des évaluations de risque (endpoint 8)
# =============================================================================
# Stockage simple dans un fichier JSON local (data/risk_evaluations_store.json).
# Suffisant pour un backend mono-instance ; à remplacer par une vraie base de
# données (PostgreSQL, SQLite...) si plusieurs instances de l'API tournent
# en parallèle (le fichier n'est pas verrouillé entre process).
# =============================================================================

library(jsonlite)

RISK_LEVELS <- list(
  list(max = 4,  label = "Faible"),
  list(max = 9,  label = "Mod\u00e9r\u00e9"),
  list(max = 15, label = "\u00c9lev\u00e9"),
  list(max = 25, label = "Tr\u00e8s \u00c9lev\u00e9")
)

niveau_risque_from_matrix_score <- function(score) {
  for (lvl in RISK_LEVELS) if (score <= lvl$max) return(lvl$label)
  "Tr\u00e8s \u00c9lev\u00e9"
}

#' Lit toutes les évaluations stockées (retourne liste vide si fichier absent)
read_risk_evaluations <- function() {
  if (!file.exists(PATH_RISK_EVAL_STORE)) return(list())
  content <- tryCatch(fromJSON(PATH_RISK_EVAL_STORE, simplifyVector = FALSE), error = function(e) list())
  content
}

#' Écrit la liste complète des évaluations sur disque.
#' NOTE: on utilise jsonlite::write_json() plutôt que write()/writeLines()
#' base R : ces dernières réencodent le texte selon la locale du système, ce
#' qui corrompt les caractères accentués (é, è...) en "<U+00E8>" quand R
#' tourne sous une locale non-UTF-8 (ex: locale "C", fréquente sur serveur).
#' write_json() écrit les octets UTF-8 directement, sans ce problème.
write_risk_evaluations <- function(evals) {
  write_json(evals, PATH_RISK_EVAL_STORE, auto_unbox = TRUE, null = "null", pretty = TRUE)
}

#' Ajoute une nouvelle évaluation à partir du corps JSON reçu en POST.
#' Génère id_evaluation et date_evaluation, recalcule score_global et
#' niveau_risque côté serveur (ne fait pas confiance aux valeurs calculées
#' côté client).
add_risk_evaluation <- function(body) {
  evals <- read_risk_evaluations()

  probabilite <- as.integer(body$probabilite)
  gravite     <- as.integer(body$gravite)
  if (is.na(probabilite) || probabilite < 1 || probabilite > 5) probabilite <- 1
  if (is.na(gravite) || gravite < 1 || gravite > 5) gravite <- 1
  score_global <- probabilite * gravite

  new_id <- sprintf("EVAL-%03d", length(evals) + 1)
  new_eval <- list(
    id_evaluation = new_id,
    date_evaluation = fmt_datetime(Sys.time()),
    evaluateur = body$evaluateur %||% NA_character_,
    evenement = body$evenement %||% NA_character_,
    maladie = body$maladie %||% NA_character_,
    district = body$district %||% NA_character_,
    region = body$region %||% NA_character_,
    probabilite = probabilite,
    gravite = gravite,
    score_global = score_global,
    niveau_risque = niveau_risque_from_matrix_score(score_global),
    contributions = body$contributions %||% list(),
    recommandations = body$recommandations %||% NA_character_
  )

  evals[[length(evals) + 1]] <- new_eval
  write_risk_evaluations(evals)
  new_eval
}
