
<!-- README.md is generated from README.Rmd. Please edit that file -->

# migR<img src="man/figures/logo_migr.png" align="right"  width=200 />

# Analyse des migrations résidentielles

L’objectif du package `migR` est de faciliter l’exploitation des
fichiers du recensement de l’Insee décrivant les migrations
résidentielles. Il propose également des indicateurs quantifiant les
échanges entre territoires à plusieurs échelles géographiques, ainsi
que des indicateurs caractérisant l’impact des migrations dans la
composition socio-démographique des territoires. Le package `migR` a été
réalisé dans le cadre de la rédaction du rapport de l’Observatoire des
Territoires portant sur les migrations résidentielles.

## Installation

Le package `migR` peut être installé avec la commande suivante :

``` r
library(devtools)
devtools::install_github("observatoire-territoires/migR")
library(migR)
```

## Documentation

Deux articles sont en ligne :

  - une [note
    méthodologique](https://observatoire-territoires.github.io/migR/articles/methodo_migr.html)
    présentant les concepts nécessaires à l’étude des migrations
    résidentielles.
  - un
    [tutoriel](https://observatoire-territoires.github.io/migR/articles/tutorial_fonctions_migr.html)
    détaillant les fonctions du package.

La principale source statistique utilisée est le [fichier détail de
l’Insee](https://www.insee.fr/fr/statistiques/4171543?sommaire=4171558)
décrivant les mobilités résidentielles. Les packages
[COGugaison](https://github.com/antuki/COGugaison) pour la gestion des
géographies et
[Insee2MonetDB](https://github.com/joelgombin/Insee2MonetDB) pour le
chargement des données ont particulièrement été mis à contribution.

### Contact

Des questions ? Des commentaires sur le package `migR` ou sur le rapport
? [Boite contact de l’Observatoire des
Territoires](mailto:observatoire@cget.gouv.fr)\!

Il est également possible de contribuer au package `migR` via une
[PR](https://github.com/observatoire-territoires/migR/pulls) ou remonter
un problème en [ouvrant une
issue](https://github.com/observatoire-territoires/migR/issues).
