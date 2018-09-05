#' @name ajout_libelles_nivgeo
#'
#' @title Ajouter le libellé d'une unité géographique
#'
#' @description Ajouter le libellé d'une unité géographique à partir du champ contenant le code en format court.
#'
#' @param TABLE Table en entrée avec un champ contenant un code d'unité géographique.
#' @param NIVGEO_IN Nom du champ de la table en entrée contenant le code d'unité géographique (en format court, liste : cf. details ci-dessous).
#' @param NIVGEO_OUT Nom du code d'unité géographique (en format court, liste : cf. details ci-dessous).
#' @param LIBGEO_OUT Nom du champ du libellé d'unité géographique à ajouter dans la table en sortie (par défaut : préfixe "LIB_").
#' @param COG_NIVGEO Millésime du Code Officiel Géographique du code d'unité géographique en entrée (liste : cf. details ci-dessous).
#'
#' @return Renvoie une table avec un champ supplémentaire contenant le libellé de l'unité géographique.
#'
#' @importFrom dplyr tribble distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_if bind_rows
#' @importFrom rlang sym
#'
#' @examples
#' # Ajout du champ 'LIB_DEP' contentant le libellé en clair du code DEP
#' indics_migres_DEP <-
#' ajout_libelles_nivgeo(TABLE = indics_migres_DEP,
#'                       NIVGEO_IN ="DEP",
#'                       COG_NIVGEO = 2018)
#'
#'# Ajout du champ 'LIB_REG_ANTE' contentant le libellé en clair des codes EPCI
#' flux_migres_EPCI <-
#'  ajout_libelles_nivgeo(TABLE = flux_migres_EPCI,
#'                        NIVGEO_IN ="EPCI_ANTE",
#'                        NIVGEO_OUT ="EPCI",
#'                        LIBGEO_OUT = "LIB_EPCI_ANTE",
#'                        COG_NIVGEO = 2018) %>%
#'   ajout_libelles_nivgeo(TABLE = .,
#'                         NIVGEO_IN ="EPCI_ACTU",
#'                         NIVGEO_OUT ="EPCI",
#'                         LIBGEO = "LIB_EPCI_ACTU",
#'                         COG_NIVGEO = 2018)
#'
#' @export
#'
#' @details
#' Les millésimes du COG disponibles sont les suivants : 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018. \cr
#'
#' Les niveaux supra-communaux disponibles sont les suivants :
#' \itemize{
#' \item{Département ('DEP')}
#' \item{Région administrative ('REG')}
#' \item{Établissement public de coopération intercommunale ('EPCI')}
#' \item{Arrondissement ('ARR')}
#' \item{Canton-ou-ville ('CV')}
#' \item{Zone d'emploi ('ZE2010')}
#' \item{Unité Urbaine ('UU2010')}
#' \item{Aire Urbaine ('AU2010')}
#' \item{Zonage en Aires Urbaines ('CATAEU2010')}
#' \item{Bassin de vie ('BV2012')}
#' \item{Type de densité selon la grille de densité communale ('TYPEDENS')}

ajout_libelles_nivgeo <- function(TABLE, NIVGEO_IN, NIVGEO_OUT , LIBGEO_OUT, COG_NIVGEO) {

  # si NIVGEO_IN pas renseigné : il prend la valeur de NIVGEO_OUT
  if( missing(NIVGEO_OUT ) ) {  NIVGEO_OUT = NIVGEO_IN  }
  else {  NIVGEO_OUT = NIVGEO_OUT }

  # si NIVGEO_IN pas renseigné : il prend la valeur de NIVGEO_OUT
  if( missing(LIBGEO_OUT ) ) {  LIBGEO_OUT = paste0("LIB_",NIVGEO_OUT)  }
  else {  LIBGEO_OUT = LIBGEO_OUT }

  # référentiels ad hoc
  ref_CATAEU2010 <-
    tribble(
      ~CATAEU2010, ~LIB_CATAEU2010,
      '111' , "Grands pôles urbains",
      '112' , "Couronne des grands pôles urbains",
      "120" , "Communes multipolarisées",
      '211' , "Moyens pôles",
      '212' , "Couronne des moyens pôles",
      '221' , "Petits pôles",
      '222' , "Couronne des petits pôles",
      '300' , "Autres communes multipolarisées",
      '400' , "Communes isolées hors influence des pôles")

  ref_GRIDDENS <-
    tribble(
      ~TYPEDENS, ~LIB_TYPEDENS,
      "1" , "Dense",
      "2" , "Densité intermédiaire",
      "3" , "Peu dense",
      "4" , "Très peu dense")

  REF_libelles <-
    get(paste0("libelles_supracom_", COG_NIVGEO)) %>%
    bind_rows(ref_CATAEU2010 %>% mutate(NIVGEO = "CATAEU2010") %>% rename(CODGEO = CATAEU2010, LIBGEO = LIB_CATAEU2010)) %>%
    bind_rows(ref_GRIDDENS %>% mutate(NIVGEO = "TYPEDENS") %>% rename(CODGEO = TYPEDENS, LIBGEO = LIB_TYPEDENS)) %>%
    # libellés communes
    bind_rows(get(paste0("table_supracom_",COG_NIVGEO)) %>% dplyr::select(CODGEO,LIBGEO) %>%  mutate(NIVGEO = "COMM") ) %>%
    rename(NIVGEO_REF = NIVGEO)


  TABLE <- TABLE %>% rename(NIVGEO_OUT = NIVGEO_IN) %>%
    left_join(REF_libelles %>% filter(NIVGEO_REF == NIVGEO_OUT) %>% dplyr::select(CODGEO, LIBGEO),
              by = c( 'NIVGEO_OUT' = 'CODGEO')  ) %>%
    #ordre variables dans dataframe
    dplyr::select(NIVGEO_OUT, LIBGEO, everything())

  # renommer variables NIVGEO et LIBGEO
  TABLE <- TABLE %>% rename(!!NIVGEO_IN := NIVGEO_OUT, !!LIBGEO_OUT := LIBGEO )

}

