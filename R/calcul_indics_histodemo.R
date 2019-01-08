#' @name calcul_indics_histodemo
#'
#' @title Calculer les indicateurs d'évolution démographique depuis 1968
#'
#' @description A partir de la base de données "séries historiques", calculer les indicateurs d'évolution démographique (évolution de la population, solde naturel, solde migratoire apparent...) par période intercensitaire et à la maille communale ou supra-communale souhaitée.
#'
#' @param TABLE Table en entrée générée par la fonction 'chargement_bd_histodemo'.
#' @param anneeRP Millésime du Recensement de la Population de l'Insee correspondant à la table (2015 par défaut).
#' @param NIVGEO Nom du niveau géographique supra-communal à ajouter (en format court, liste : cf. details ci-dessous).
#' @param COG_NIVGEO Millésime du Code Officiel Géographique du code supra-communal en sortie (liste : cf. details ci-dessous).
#'
#' @return Renvoie une table contenant les indicateurs d'évolution démographique par territoire et par période intercensitaire.
#'
#' @importFrom dplyr distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_if vars funs
#' @importFrom rlang sym
#' @importFrom tidyr spread gather
#' @import COGugaison
#' @importFrom magrittr set_colnames
#'
#' @examples
#' \dontrun{
#' # Calculer les indicateurs d'évolution démographique par période entre 1968 et 2015, à la maille département
#' DEP_histodemo_19682015 <-  calcul_indics_histodemo(TABLE = COMM_HISTODEMO_2015, anneeRP = 2015, NIVGEO = "DEP",COG_NIVGEO = 2018)
#' }
#'
#' @details
#'
#' Les champs de la table en sortie sont les suivants :
#' \itemize{
#' \item{'periode' : période intercensitaire concernée, format "AAAA_AAAA"}
#' \item{'periode_milieu' : milieu de la période intercensitaire concernée, format numérique}
#' \item{'periode_nb_annees' : nombre d'années de la période intercensitaire concernée}
#' \item{'POPULATION_debut' : Population en début de période}
#' \item{'POPULATION_fin' : Population en fin de période}
#' \item{'DECES' : Nombre de décès au cours de la période}
#' \item{'NAISSANCES' : Nombre de naissances au cours de la période}
#' \item{'EVOL_DEMO_TOT' : Evolution démographique au cours de la période}
#' \item{'EVOL_DEMO_SN' : Evolution démographique due au solde naturel au cours de la période}
#' \item{'EVOL_DEMO_SMA' : Evolution démographique due au solde migratoire apparent au cours de la période}
#' \item{'TX_EVOL_DEMO_AN_TOT' : Taux annuel d'évolution démographique au cours de la période}
#' \item{'TX_EVOL_DEMO_AN_SN' : Taux annuel d'évolution démographique due au solde naturel au cours de la période}
#' \item{'TX_EVOL_DEMO_AN_SMA' : Taux annuel d'évolution démographique due au solde migratoire apparent au cours de la période}}
#'
#'
#' Les millésimes du COG disponibles sont les suivants : 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018. \cr
#'
#' Pour convertir un code commune d'un COG plus ancien, il est conseillé d'effectuer l'opération grâce au package 'COGugaison'. \cr
#'
#' Les niveaux supra-communaux disponibles sont les suivants :
#' \itemize{
#' \item{Commune ('COMM')}
#' \item{Département ('DEP')}
#' \item{Région administrative ('REG')}
#' \item{Établissement public de coopération intercommunale ('EPCI')}
#' \item{Nature d'Établissement public de coopération intercommunale ('NATURE_EPCI')}
#' \item{Arrondissement ('ARR')}
#' \item{Canton-ou-ville ('CV')}
#' \item{Zone d'emploi ('ZE2010')}
#' \item{Unité Urbaine ('UU2010')}
#' \item{Tranche d'Unité Urbaine ('TUU2015')}
#' \item{Tranche détaillée d'Unité Urbaine ('TDUU2015')}
#' \item{Aire Urbaine ('AU2010')}
#' \item{Tranche d'aire urbaine ('TAU2015')}
#' \item{Zonage en Aires Urbaines ('CATAEU2010')}
#' \item{Bassin de vie ('BV2012')}
#' \item{Type de densité selon la grille de densité communale ('TYPEDENS')}}
#'
#' @references
#' \itemize{
#' \item{\href{https://antuki.github.io/COGugaison/articles/COGugaison.html}{Package 'COGugaison' pour gérer les changements de COG}}
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{Historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{Tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#'
#' @export
#'


calcul_indics_histodemo <- function(TABLE, anneeRP =2015, NIVGEO, COG_NIVGEO) {

  # définition du COG du fichier en entrée
  COG_IN <- anneeRP + 2


  if(COG_IN == 2016) {
    COMM_GRIDENS_OK <- COMM_GRIDENS
  }
  else{
    COMM_GRIDENS_OK <- changement_COG_typo(table_entree=COMM_GRIDENS ,
                                           annees=c(2016:COG_IN),
                                           methode_fusion="methode_max_pop",libgeo=F,donnees_insee=T)
  }

  #### création de la table de correspondance supra-communale ####
  # cas 1 : les COG d'entrée et de sortie sont identiques
  if(COG_IN == COG_NIVGEO) {

    # table de correspondance maillages supra comm
    table_supracom_OK <- get(paste0("table_supracom_", as.character(COG_IN))) %>%
      select(-LIBGEO) %>%
      # autres mailes supra-communales
      mutate(COMM = CODGEO,
             PAYS = "FRA",
             METRODOM = case_when(substr(CODGEO,1,2) %in% '97' ~ "DOM", TRUE ~ "METRO")) %>%
      left_join(COMM_GRIDENS_OK, by = c('CODGEO', "CODGEO"))


  }

  # cas 2 : les COG d'entrée et de sortie sont différents
  else {

    table_supracom_IN <- get(paste0("table_supracom_", as.character(COG_IN)) )
    table_supracom_NIVGEO <- get(paste0("table_supracom_", as.character(COG_NIVGEO)) )


    table_supracom_OK <- table_supracom_IN %>% select(CODGEO) %>%
      left_join(
        changement_COG_typo(table_entree=table_supracom_NIVGEO %>% select(-LIBGEO),
                            annees=c(COG_NIVGEO:COG_IN),
                            methode_fusion="methode_max_pop",libgeo=F,donnees_insee=T),
        by = "CODGEO") %>%
      # autres mailles supra-communales
      mutate(COMM = CODGEO,
             PAYS = "FRA",
             METRODOM = case_when(substr(CODGEO,1,2) %in% '97' ~ "DOM", TRUE ~ "METRO")) %>%
      left_join(COMM_GRIDENS_OK, by = c('CODGEO', "CODGEO"))

  }


  # !!sym(paste0("POP_",last_RP))
  #anneeRP
  anneeRP_prec <- anneeRP - 5

  NIVGEO_indics_histodemo <-
    TABLE %>%
    left_join(table_supracom_OK, by = c("CODGEO" = "CODGEO")) %>%
    mutate(#POPDEB1015 = POP_2010, POPFIN1015 = POP_2015,
      !!sym(paste0("POPDEB",substr(anneeRP_prec,3,4),substr(anneeRP,3,4))) := !!sym(paste0("POP_",anneeRP_prec)),
      !!sym(paste0("POPFIN",substr(anneeRP_prec,3,4),substr(anneeRP,3,4))) := !!sym(paste0("POP_",anneeRP)),
      #POPDEB9910 = POP_1999, POPFIN9910 = POP_2010,
      !!sym(paste0("POPDEB99",substr(anneeRP_prec,3,4))) := POP_1999,
      !!sym(paste0("POPFIN99",substr(anneeRP_prec,3,4))) := !!sym(paste0("POP_",anneeRP_prec)),
      POPDEB9099 = POP_1990, POPFIN9099 = POP_1999,
      POPDEB8290 = POP_1982, POPFIN8290 = POP_1990,
      POPDEB7582 = POP_1975, POPFIN7582 = POP_1982,
      POPDEB6875 = POP_1968, POPFIN6875 = POP_1975)  %>%
    dplyr::select(NIVGEO,
                  #ends_with("1015"),
                  ends_with(paste0(substr(anneeRP_prec,3,4),substr(anneeRP,3,4))),
                  #ends_with("9910"),
                  ends_with(paste0( "99",substr(anneeRP_prec,3,4))),
                  ends_with("9099"),ends_with("8290"),ends_with("7582"),ends_with("6875"))  %>%
    group_by(!!rlang::sym(NIVGEO)) %>%
    summarise_if(is.numeric,funs(sum)) %>%
    gather(indic,POP, -!!rlang::sym(NIVGEO)) %>%
    mutate(periode = str_sub(indic,-4,-1),
           indic = substr(indic,1,nchar(indic)-4)) %>%
    spread(indic,POP) %>%
    mutate(annee_debut = case_when(substr(periode,1,2) <20 ~ as.numeric(paste0(20,substr(periode,1,2))),
                                   TRUE ~ as.numeric(paste0(19,substr(periode,1,2))) )) %>%
    mutate(annee_fin = case_when(str_sub(periode,-2,-1) <20 ~ as.numeric(paste0(20,str_sub(periode,-2,-1))),
                                 TRUE ~ as.numeric(paste0(19,str_sub(periode,-2,-1))) )) %>%
    mutate(EVOLPOP = POPFIN - POPDEB,
           SN = NAIS - DECE,
           SM = EVOLPOP - SN) %>%
    mutate(nb_annees = annee_fin - annee_debut,
           TCAMPOP = (POPFIN / POPDEB) ^(1/nb_annees)-1,
           TCAMPOP_SN = SN * TCAMPOP / EVOLPOP,
           TCAMPOP_SM = TCAMPOP - TCAMPOP_SN) %>%
    mutate(periode_annees = paste0(annee_debut,"_",annee_fin),
           periode_milieu = annee_fin - (annee_fin - annee_debut)/2) %>%
    # ordres des variables
    select(!!rlang::sym(NIVGEO),
           periode = periode_annees, periode_milieu, periode_nb_annees = nb_annees,
           POPULATION_debut = POPDEB,
           POPULATION_fin = POPFIN,
           DECES = DECE,
           NAISSANCES = NAIS,
           EVOL_DEMO_TOT =  EVOLPOP,
           EVOL_DEMO_SN = SN,
           EVOL_DEMO_SMA = SM,
           TX_EVOL_DEMO_AN_TOT = TCAMPOP,
           TX_EVOL_DEMO_AN_SN = TCAMPOP_SN,
           TX_EVOL_DEMO_AN_SMA = TCAMPOP_SM)

}

