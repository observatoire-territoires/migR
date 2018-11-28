#' @name creation_carto_supracomm
#'
#' @title Générer la cartographie d'un maillage supracommunal
#'
#' @description A partir d'un fichier cartographiquee des communes françaises (issu du produit AdminExpress (http://www.professionnels.ign.fr/adminexpress) de l'IGN par exemple), générer la cartographie correspondant à un maillage supracommunal (Département, aire urbaine, etc...) sous forme de polygones ou de centroides.
#' @title Ajouter le libellé d'une modalité de variable de ventilation
#'
#' @description A partir d'une cartographie des communes françaises (issu du produit AdminExpress (http://www.professionnels.ign.fr/adminexpress) de l'IGN par exemple), générer la cartographie correspondant à un maillage supracommunal (Département, aire urbaine, etc...) sous forme de polygones ou de centroides.
#'
#' @param CARTO_COMM Cartographie en entrée des communes françaises, format sf dataframe.
#' @param CODE_COMMUNE Nom du champ contenant l'identifiant communal (de type caractère) de la table en entrée.
#' @param COG_IN Millésime du Code Officiel Géographique des communes en entrée.
#' @param COG_NIVGEO Millésime du Code Officiel Géographique du maillage supra-communal en sortie.
#' @param NIVGEO Nom du code d'unité géographique à générer en sortie (en format court, liste : cf. details ci-dessous).
#' @param FORMAT Format de la cartographie générée : polygones ("poly") ou centroides ("ctr").
#' @param SG Niveau de généralisation de la géometrie générée : pourcentage des sommets de la géométrie initiale conservés, 1 par défaut.
#'
#'
#' @return Renvoie la cartographie du maillage supracommunal sous forme d'objet sf dataframe.
#'
#' @importFrom dplyr tribble distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_all bind_rows everything
#' @importFrom rlang sym
#' @importFrom rmapshaper ms_simplify
#' @importFrom magrittr set_colnames
#' @import COGugaison
#' @import sf
#'
#' @examples
#' \dontrun{
#' # Création de la cartographie des anciennes régions (millésime 2015) en format polygones avec géométrie généralisée
#' geo_REG_poly <-
#' creation_carto_SUPRACOMM(CARTO_COMM = COMMG_COG2016_METDOM,
#'                          CODE_COMMUNE = "DEPCOM",
#'                          COG_IN = 2016,
#'                          COG_NIVGEO = 2015,
#'                          NIVGEO = "REG",
#'                          FORMAT = "poly",
#'                          SG = 0.05)
#'
#'# Création de la cartographie des EPCI (millésime 2018) en format centroïdes
#' geo_EPCI_ctr <-
#' creation_carto_SUPRACOMM(CARTO_COMM = COMMG_COG2016_METDOM,
#'                          CODE_COMMUNE = "DEPCOM",
#'                          COG_IN = 2016,
#'                          COG_NIVGEO = 2018,
#'                          NIVGEO = "EPCI",
#'                          FORMAT ="ctr",
#'                          SG = 1)
#'}
#'
#' @details
#' La conversion entre les différents millésimes du COG est gérée grâce à la fonction 'changement_COG_typo' du package 'COGugaison' ; la méthode de fusion par défaut est 'methode_max_pop' (cf. aide "COGugaison").
#'
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
#' \item{Type de densité selon la grille de densité communale ('TYPEDENS')}}
#'
#' @export
#'


creation_carto_supracomm <- function(CARTO_COMM,CODE_COMMUNE,COG_IN,COG_NIVGEO,NIVGEO,FORMAT="poly",SG=1) {

  # format sf dataframe
  class(CARTO_COMM) = c("sf","data.frame")

  # 1 - gestion des arrondissements PLM ####

  # tester si le champ CODE_COMMUNE contient des arrondissements PLM
  test_ARRPLM_CODE_COMMUNE <-  is.element(  as.character(c(seq(from=13201, to=13216, by = 1), seq(from=69381, to=69389, by = 1), seq(from=75101, to=75120, by = 1))), CARTO_COMM %>% as.data.frame() %>% distinct(!!sym(CODE_COMMUNE)) %>% pull() )

  # si le champ CODE_COMMUNE contient au moins 1 arrondissement PLM on normalise les codes communes sans arrondissements PLM
  if( is.element(TRUE, test_ARRPLM_CODE_COMMUNE) == TRUE ) {

    CARTO_COMM <- CARTO_COMM %>%  mutate(!!sym(paste0(CODE_COMMUNE, "_sansARRPLM")) := !!sym(CODE_COMMUNE))
    CARTO_COMM <- enlever_PLM(CARTO_COMM, codgeo_entree = paste0(CODE_COMMUNE, "_sansARRPLM"), libgeo = NULL, agregation = F)

  }
  # si le champ CODE_COMMUNE ne contient pas d'arrondissements PLM on conserve les champs tels quels

  else {
    CARTO_COMM <- CARTO_COMM %>%  mutate(!!sym(paste0(CODE_COMMUNE, "_sansARRPLM")) := !!sym(CODE_COMMUNE))
  }

  # 2 - table supra communale ####

  # conversion référentiel grille de densité communale ####
  #https://www.insee.fr/fr/information/2114627

  if(COG_IN == 2016) {
    COMM_GRIDENS_OK <- COMM_GRIDENS %>% as.data.frame()
  }
  else {
    COMM_GRIDENS_OK <- changement_COG_typo(table_entree=COMM_GRIDENS %>% as.data.frame() ,
                                           annees=c(2016:COG_IN),
                                           methode_fusion="methode_max_pop",
                                           libgeo=F,
                                           donnees_insee=T)
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

  #### jointure du code supra-communal dans la couche communale

  CARTO_COMM_new <- CARTO_COMM %>%
    left_join(table_supracom_OK %>%
                select(CODGEO, NIVGEO) %>%
                set_colnames(c(paste0(CODE_COMMUNE, "_sansARRPLM"),NIVGEO)),
              by = paste0(CODE_COMMUNE, "_sansARRPLM") ) %>%
    select(-!!paste0(CODE_COMMUNE, "_sansARRPLM"))

  # aggrégation de la table supra-comm
  NIVGEO_geo <- CARTO_COMM_new %>%  dplyr::select(!!rlang::sym(NIVGEO)) %>% dplyr::group_by(!!rlang::sym(NIVGEO)) %>% dplyr::summarize()

  # sortie sous format polygones ou centroides

  if(FORMAT %in% "poly") {
    # simplification des geometries
    NIVGEO_geo.s <- ms_simplify(input = as(NIVGEO_geo, 'Spatial'), keep = SG) %>% st_as_sf() %>% ungroup()
    out <- NIVGEO_geo.s %>% st_set_crs(st_crs(CARTO_COMM))
  }

  else if(FORMAT %in% "ctr")  {
    # centroides
    NIVGEO_geo.ctr <- NIVGEO_geo %>% st_centroid( of_largest_polygon = F) %>%
      mutate(x_ctr = map_dbl(geometry, ~st_centroid(.x)[[1]]),
             y_ctr = map_dbl(geometry, ~st_centroid(.x)[[2]]))

    out <- NIVGEO_geo.ctr %>% st_set_crs(st_crs(CARTO_COMM))
  }

  # sortie
  out

}


