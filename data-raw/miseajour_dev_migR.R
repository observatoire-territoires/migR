library(pkgdown)
library(roxygen2)
library(devtools)

#générer doc .Rd
roxygen2::roxygenise()


#vignettes :
devtools::build_vignettes()

# pour générer les articles issues des vignettes
pkgdown::build_articles(pkg = ".")


# vérification de l'intégrité du package
devtools::check()

