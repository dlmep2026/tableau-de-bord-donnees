# API Backend SIMR — R Plumber

Backend R (Plumber) pour le tableau de bord de Surveillance Épidémiologique
MINSANTE / DLMEP / CCOUSP, implémentant les 10 endpoints de
`besoin_specification_api_backend.md`.

**Ce projet a été testé de bout en bout** (données réelles chargées, serveur
Plumber démarré, les 10 endpoints interrogés en HTTP et validés en JSON) —
pas seulement écrit. Voir "Limites connues" ci-dessous pour ce qui reste à
faire avant une mise en production.

---

## 1. Installation & démarrage

```bash
# Dépendances système (Debian/Ubuntu) — pour le package sf
sudo apt-get install -y libgdal-dev libgeos-dev libproj-dev libudunits2-dev

# Packages R
Rscript -e 'install.packages(c("plumber","readxl","dplyr","tidyr","stringr","stringi","jsonlite","sf","purrr"))'

# Démarrage (depuis la racine du projet, là où se trouve plumber.R)
Rscript start_api.R
# -> API disponible sur http://0.0.0.0:8000/api/v1
# -> Documentation Swagger auto-générée sur http://<host>:8000/__docs__/
```

## 2. Structure du projet

```
simr_api/
├── data/                          # Sources de données (copies des fichiers fournis)
│   ├── overall_mape.xlsx          # Notifications hebdomadaires (cas/décès/complétude)
│   ├── LL_MPOX.xlsx                # Linelist Mpox
│   ├── LL_CHOLERA_NATIONAL.xlsx    # Linelist Choléra
│   ├── epidemie.xlsx               # Statut alerte/épidémie déclaré par district
│   ├── region.geojson              # Géométries des 10 régions
│   ├── district.geojson            # Géométries des districts (PARTIEL, voir §4)
│   └── risk_evaluations_store.json # Créé automatiquement au 1er POST /risk-evaluations
├── R/
│   ├── 00_config.R                 # Chemins, mapping maladies, seuils par défaut
│   ├── 01_utils.R                  # Normalisation texte, parsing semaines, helpers
│   ├── 02_data_loader.R            # Lecture + cache des sources brutes
│   ├── 03_district_reference.R     # Référentiel districts + règle de propagation
│   ├── 04_case_data.R              # Linelists unifiés + pyramide âge/sexe
│   ├── 05_kpis_series.R            # KPIs, courbe épidémique, ventilations, résumé
│   ├── 06_alerts.R                 # Alertes dérivées des seuils (voir §4)
│   ├── 07_risk_store.R             # Persistance JSON des évaluations de risque
│   └── 08_at_risk.R                # Priorisation des districts à risque
├── plumber.R                       # Routeur : CORS + les 10 endpoints
└── start_api.R                     # Script de démarrage
```

## 3. Mapping des données sources -> endpoints

| Endpoint | Source(s) principale(s) |
|---|---|
| 1. `/status/sync` | Horodatage des fichiers sur disque (proxy, voir §4) |
| 2. `/indicators` | `overall_mape.xlsx` (cas/décès/complétude/promptitude) + `epidemie.xlsx` |
| 3. `/districts/map` | `epidemie.xlsx` + `district.geojson`/`region.geojson` + `overall_mape.xlsx` |
| 4. `/epidemiology/series` | `overall_mape.xlsx` (cas/décès) + linelists (part confirmée) |
| 5. `/epidemiology/breakdowns` | `overall_mape.xlsx` + `epidemie.xlsx` |
| 6. `/alerts` | **Dérivé** de `overall_mape.xlsx` (voir §4, pas de registre fourni) |
| 7. `/epidemiology/age-sex-pyramid` | `LL_MPOX.xlsx` + `LL_CHOLERA_NATIONAL.xlsx` uniquement |
| 8. `/risk-evaluations` | Stockage JSON local (`data/risk_evaluations_store.json`) |
| 9. `/districts/at-risk` | Même pipeline que l'endpoint 3, reclassé |
| 10. `/home/disease-summary` | `overall_mape.xlsx` + `epidemie.xlsx` |

Les 4 maladies du frontend sont mappées aux colonnes `variable` de
`overall_mape.xlsx` dans `R/00_config.R::DISEASE_MAP` :
`Cholera→cholera`, `Mpox→variole_du_singe_monkey_pox`, `FHV→fhv`,
`Syndrome-Grippal→syndrome_grippal`.

## 4. Hypothèses et limites connues (IMPORTANT)

Plusieurs sources mentionnées comme nécessaires dans la spec n'ont pas été
fournies. Le code reste fonctionnel grâce aux règles de repli documentées
ci-dessous, mais elles doivent être validées/remplacées avec l'équipe
DLMEP/CCOUSP avant mise en production.

- **`district.geojson` est partiel** : il ne contient que 33 districts, tous
  en région Centre (sur 206 attendus). L'adjacence géographique (et donc la
  règle de propagation épidémie → alerte) n'est donc calculable
  précisément que pour ces 33 districts. Pour les autres, le centroïde
  affiché sur la carte est celui de leur **région** (repli), et la
  population est `null` (donc `taux_attaque` est `null`, pas d'erreur mais
  pas de valeur). **Dès que le geojson national complet est disponible,
  il suffit de remplacer `data/district.geojson` — aucun changement de
  code n'est nécessaire.**

- **Aucun registre d'alertes** (endpoint 6) : le champ `Registre des
  alertes et investigations` mentionné dans la spec n'a pas été fourni.
  Les alertes sont donc **synthétisées** à partir de `overall_mape.xlsx` :
  chaque (district, semaine, maladie) dont le nombre de cas franchit un
  seuil (voir `DEFAULT_THRESHOLDS` dans `00_config.R`) génère une entrée.
  Les champs `fosa`, `source`, `statut_traitement`, `investigation_dans_48h`
  n'ont pas de source réelle et sont déduits par des règles simples
  documentées dans `R/06_alerts.R`. À remplacer par une lecture directe
  dès qu'un vrai registre existe.

- **Seuils d'alerte/épidémiques par défaut** : aucun fichier de seuils par
  maladie et par district n'a été fourni. Des seuils par défaut, par
  maladie uniquement (pas par district), sont appliqués
  (`DEFAULT_THRESHOLDS` dans `00_config.R`). Facile à remplacer par une
  table district×maladie si elle devient disponible.

- **Score de risque (`score_risque`, districts carte + priorisation)** :
  aucune formule n'était fournie dans la spec. Formule composite 0–100
  utilisée (voir `R/03_district_reference.R::compute_score_risque`) :
  40 pts si Épidémie / 20 si Alerte, + jusqu'à 30 pts selon le taux
  d'attaque, + jusqu'à 20 pts selon la létalité, + 10 pts si limitrophe
  d'une épidémie. Seuils de `niveau_risque` : Faible ≤24, Modéré 25–49,
  Élevé 50–74, Très Élevé ≥75. **À valider avec l'équipe épidémiologie.**

- **"Valeur précédente" des KPIs** (endpoint 2) : comparée sur une fenêtre
  de même largeur immédiatement précédente (ex. semaines sélectionnées
  S30–S35 → comparé à S24–S29). Si `semaine_debut` n'est pas fourni, la
  période = `[1, semaine_fin]` (cumul depuis le début de l'année) et la
  période précédente = `[1, semaine_debut-1]`. Voir
  `R/05_kpis_series.R` en tête de fichier.

- **Pyramide âge/sexe (endpoint 7)** : seuls Cholera et Mpox disposent
  d'un linelist individuel. FHV et Syndrome-Grippal renverront toujours
  une pyramide à zéro (structure correcte, valeurs nulles) tant qu'aucun
  linelist n'existe pour ces maladies.

- **Cas "confirmé" vs "suspect"** dans les linelists :
  - Mpox : confirmé si `Classification finale == "CONFIRME"` ; les lignes
    `"NON-CAS"` sont exclues entièrement.
  - Choléra : confirmé si RDT positif OU culture positive ; toute ligne
    notifiée est comptée comme suspecte (définition de cas standard).

- **`/status/sync`** : n'ayant pas d'accès temps réel à DHIS2 / Ligne
  Verte 1510 / surveillance communautaire, la date de dernière
  modification des fichiers sources sur disque est utilisée comme proxy
  de "dernière synchronisation", et `records_count`/`calls_count`/
  `reports_count` reflètent le nombre de lignes lues dans les fichiers
  correspondants (au lieu de compteurs d'appels API réels).

- **Persistance des évaluations de risque** : stockage fichier JSON local
  (non transactionnel, non verrouillé). Suffisant pour un backend
  mono-instance ; passer à une vraie base de données si l'API est
  déployée en plusieurs instances.

## 5. Points techniques notables

- **Locale/encodage** : le code force `sf::sf_use_s2(FALSE)` (les
  géométries sources contiennent des topologies imparfaites que le moteur
  sphérique strict `s2` rejette) et utilise `stringi` plutôt qu'`iconv`
  pour la suppression d'accents (comportement d'`iconv` dépendant de la
  locale système, peu fiable). `start_api.R` tente aussi de forcer une
  locale UTF-8 au démarrage.
- **Sérialisation JSON** : le serializer global est
  `serializer_unboxed_json(na = "null")` — sans cela, jsonlite renvoie par
  défaut les scalaires encapsulés dans des tableaux (`["valeur"]` au lieu
  de `"valeur"`) et les `NA` sous forme de chaîne `"NA"` au lieu de `null`.
- **Champs tableau toujours-tableau** : `maladies_actives`,
  `districts_limitrophes_en_epidemie`, `districts_en_epidemie_adjacents`
  et `actions_prioritaires` sont explicitement enveloppés avec `I()` pour
  garantir un tableau JSON même à longueur 1 (sinon `auto_unbox` les
  réduirait à une simple chaîne).
- **Normalisation région/district** : chaque fichier source orthographie
  différemment les régions/districts (accents, tirets, préfixes
  "Region "/"District "...). `R/01_utils.R::normalize_text()` +
  `canonical_region_name()` ramènent tout vers une clé commune et vers le
  libellé exact attendu par le frontend (`REGIONS_CAMEROUN`).

## 6. Tests effectués

Le pipeline complet a été exécuté avec R 4.3 + les données réelles
fournies : chargement des 6 fichiers sources, calcul d'adjacence
géographique réel (`sf::st_touches`), règle de propagation, puis les 10
endpoints ont été appelés en HTTP et leur JSON validé. Plusieurs bugs ont
été détectés et corrigés à cette occasion (voir historique de
conversation) : parsing de semaines mal aligné, géométries invalides
cassant `st_centroid`, désynchronisation de région Extrême-Nord/Nord-Ouest
entre sources, colonnes manquantes dans la table de cas unifiée,
sérialisation JSON (scalaires encapsulés, `NA` en chaîne), corruption
d'accents en écriture disque, et propagation d'épidémie mal filtrée pour
les districts déjà en épidémie.

Note : `overall_mape.xlsx` fourni ne contient qu'un échantillon (32 lignes
= 1 district × 1 semaine × ~32 variables). Les endpoints s'exécutent et
répondent correctement, mais la plupart des totaux affichés seront à 0
tant qu'un fichier `overall_mape.xlsx` complet (toutes semaines, tous
districts) n'est pas fourni — ce n'est pas un bug, c'est un reflet fidèle
des données disponibles.
