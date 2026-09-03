# =============================================================================
# start_api.R — Démarrage du serveur R Plumber (SIMR API Backend)
# =============================================================================
# Lancement : Rscript start_api.R
# (exécuter depuis la racine du projet simr_api/, pour que les chemins
#  relatifs "R/" et "data/" soient résolus correctement)
# =============================================================================

required_pkgs <- c("plumber", "readxl", "dplyr", "tidyr", "stringr",
                    "jsonlite", "sf", "purrr", "stringi")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop(
    "Packages R manquants : ", paste(missing_pkgs, collapse = ", "), "\n",
    "Installez-les avec : install.packages(c(",
    paste(sprintf('"%s"', missing_pkgs), collapse = ", "), "))"
  )
}

# Tente de forcer une locale UTF-8 : certains environnements serveur
# (conteneurs minimaux notamment) démarrent en locale "C", ce qui casse le
# rendu des caractères accentués dans certaines fonctions base R (write(),
# writeLines()...). Best-effort : ne bloque pas le démarrage si indisponible
# (le code utilise par ailleurs stringi/jsonlite qui ne dépendent pas de la
# locale système pour l'essentiel des traitements).
try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_ALL", "C.UTF-8"), silent = TRUE)

# Délai max pour les téléchargements GitHub (voir R/02_data_loader.R
# ::download_github_sources()). Augmentez si votre réseau est lent ou si
# les fichiers sont volumineux.
options(timeout = 120)

library(plumber)

pr <- pr("plumber.R")
pr <- pr_set_serializer(pr, serializer_unboxed_json(na = "null"))
pr_run(pr, host = "0.0.0.0", port = 8000)
