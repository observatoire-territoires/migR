#' @name ajout_nivgeo_supracomm
#'
#' @title Ajouter un champ contenant un niveau géographique supra-communal
#'
#' @description Dans une table contenant un code commune Insee (avec ou sans arrondissements pour Paris, Lyon et Marseille), accoler à cette table un nouveau champ contenant le niveau supra-communal correspondant à cette commune.
#'
#' @param TABLE Table en entrée disposant a minima d'un champ de codes commune Insee.
#' @param CODE_COMMUNE Nom du champ de la table en entrée contenant le code commune Insee. Il peut contenir des codes arrondissements pour Paris, Lyon et Marseille (75101, 75102, etc...) ou non ('75056')
#' @param SUFFIXE Terme à ajouter en suffixe au nom du champ de niveau supra-communal ajouté à la table (facultatif).
#' @param COG_IN Millésime du Code Officiel Géographique du code commune en entrée (liste : cf. details ci-dessous). Le millésime du COG est indiqué dans les métadonnées des fichiers Insee, dans le fichier du RP d'année N il est généralement égal à N-2.
#' @param COG_NIVGEO Millésime du Code Officiel Géographique du code supra-communal en sortie (liste : cf. details ci-dessous).
#' @param NIVGEO Nom du niveau géographique supra-communal à ajouter (en format court, liste : cf. details ci-dessous). Peut être un vecteur contenant les noms de plusieurs niveaux.
#'
#' @return Renvoie une table avec un champ supplémentaire contenant le niveau géographique supra-communal.
#'
#' @importFrom dplyr distinct pull mutate select left_join case_when
#' @importFrom rlang sym
#' @import COGugaison
#' @importFrom magrittr set_colnames
#'
#' @examples
#' # Ajout du champ 'DEP' indiquant le niveau géographique "département" à une table ayant une information communale
#' COM_2012 <-
#' ajout_nivgeo_supracomm(TABLE = COM_2012 ,
#' CODE_COMMUNE = "CODGEO",
#' NIVGEO = "DEP",
#' COG_IN = 2010,
#' COG_NIVGEO = 2018)
#'
#' # Ajout des niveaux géographiques "département", "région" et "type de densité" depuis les champs "commune de résidence actuelle" et "commune de résidence antérieure"
#' FD_MIGCOM_2014  <-
#' ajout_nivgeo_supracomm(TABLE = FD_MIGCOM_2014 ,
#'                        CODE_COMMUNE = "DCRAN",
#'                        SUFFIXE = "ANTE",
#'                        NIVGEO= c("DEP",'EPCI','TYPEDENS'),
#'                        COG_IN = 2016, COG_NIVGEO = 2018) %>%
#' ajout_nivgeo_supracomm(TABLE = . ,
#'                        CODE_COMMUNE = "COMMUNE",
#'                        SUFFIXE = "ACTU",
#'                        NIVGEO= c("DEP",'EPCI','TYPEDENS'),
#'                        COG_IN = 2016, COG_NIVGEO = 2018)
#'
#'
#'
#' @details
#' Les millésimes du COG disponibles sont les suivants : 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018. \cr
#'
#' Pour convertir un code commune d'un COG plus ancien, il est conseillé d'effectuer l'opération grâce au package 'COGugaison'. \cr
#'
#' Les niveaux supra-communaux disponibles sont les suivants :
#' \itemize{
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
#' \item{Type de densité selon la grille de densité communale ('TYPEDENS')}
#'
#' @references
#' \itemize{
#' \item{\href{https://antuki.github.io/COGugaison/articles/COGugaison.html}{Package 'COGugaison' pour gérer les changements de COG}}
#' \item{\href{https://www.insee.fr/fr/information/2666684#titre-bloc-11}{Historique des géographies communales (Insee)}}
#' \item{\href{https://www.insee.fr/fr/information/2028028}{Tables d'appartenance des communes aux différents niveaux géographiques (Insee)}}}
#'
#' @export
#'


ajout_nivgeo_supracomm <- function (TABLE, CODE_COMMUNE, SUFFIXE='', COG_IN, COG_NIVGEO, NIVGEO){

  # tester si le champ CODE_COMMUNE contient des arrondissements PLM
  test_ARRPLM_CODE_COMMUNE <-  is.element(  as.character(c(seq(from=13201, to=13216, by = 1), seq(from=69381, to=69389, by = 1), seq(from=75101, to=75120, by = 1))), TABLE %>% distinct(!!sym(CODE_COMMUNE)) %>% pull() )

  # si le champ CODE_COMMUNE contient au moins 1 arrondissement PLM on normalise les codes communes sans arrondissements PLM
  if( is.element(TRUE, test_ARRPLM_CODE_COMMUNE) == TRUE ) {

    TABLE <- TABLE %>%  mutate(!!sym(paste0(CODE_COMMUNE, "_sansARRPLM")) := !!sym(CODE_COMMUNE))
    TABLE <- enlever_PLM(TABLE, codgeo_entree = paste0(CODE_COMMUNE, "_sansARRPLM"), libgeo = NULL, agregation = F)


  }
  # si le champ CODE_COMMUNE ne contient pas d'arrondissements PLM on conserve les champs tels quels

  else {
    TABLE <- TABLE %>%  mutate(!!sym(paste0(CODE_COMMUNE, "_sansARRPLM")) := !!sym(CODE_COMMUNE))
  }

  # conversion référentiel grille de densité communale ####
  #https://www.insee.fr/fr/information/2114627

  if(COG_IN == 2016) {
    COMM_GRIDENS_OK <- COMM_GRIDENS
  }
  else {
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

  #### jointure du code supra-communal dans la table ####

  if(SUFFIXE == '') {
    TABLE_new <- TABLE %>%
      left_join(table_supracom_OK %>%
                  select(CODGEO, NIVGEO) %>%
                  set_colnames(c(paste0(CODE_COMMUNE, "_sansARRPLM"),NIVGEO)),
                by = paste0(CODE_COMMUNE, "_sansARRPLM") ) %>%
      select(-!!paste0(CODE_COMMUNE, "_sansARRPLM"))

  }
  else {
    TABLE_new <- TABLE %>%
      left_join(table_supracom_OK %>%
                  select(CODGEO, NIVGEO) %>%
                  set_colnames(c(paste0(CODE_COMMUNE, "_sansARRPLM"),paste0(NIVGEO,"_",SUFFIXE))),
                by = paste0(CODE_COMMUNE, "_sansARRPLM") ) %>%
      select(-!!paste0(CODE_COMMUNE, "_sansARRPLM"))
  }

}

