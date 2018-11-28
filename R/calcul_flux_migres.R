#' @name calcul_flux_migres
#'
#' @title Calculer le volume de migrations résidentielles entre zones
#'
#' @description A partir d'une table en entrée contenant un champ de résidence actuelle et un champ de résidence antérieure sur le même niveau géographique.
#'
#' @param TABLE Table en entrée contenant un champ de résidence actuelle et un champ de résidence antérieure sur le même niveau géographique (communes, départements, etc...).
#' @param VAR_NB Nom du champ de la table en entrée contenant l'indicateur de pondération du tuple.
#' @param VAR_VENTIL Nom du champ de la variable de ventilation de la population, de type tranche d'âge, groupe socio-professionnel, etc... (facultatif).
#' @param MIG_NET_INTERNE Vaut TRUE si le calcul doit se faire uniquement sur les zones présentes à la fois en résidence actuelle et en résidence antérieure.
#' @param NIVGEO_ANTE Nom du champ de la table en entrée contenant le niveau géographique de résidence antérieure.
#' @param NIVGEO_ACTU Nom du champ de la table en entrée contenant le niveau géographique de résidence actuelle.
#'
#' @return Renvoie une table contenant un champ de résidence actuelle, un champ de résidence antérieure et un champ du nombre de migrations résidentielles entre chaque coupe de zones.
#'
#' @importFrom dplyr distinct pull mutate select left_join case_when group_by summarise ungroup
#' @importFrom rlang sym
#'
#' @examples
#' \dontrun{
#' # Calcul de la table de nombre de migrations entre résidence actuelle et résidence antérieure au niveau départemental
#' flux_migres_DEP <-
#' calcul_flux_migres(TABLE =FD_MIGCOM_2015 ,
#'                  VAR_NB = "IPONDI",
#'                  MIG_NET_INTERNE=TRUE,
#'                  NIVGEO_ANTE ="DEP_ANTE",
#'                  NIVGEO_ACTU ="DEP_ACTU")
#' # Calcul de la table de nombre de migrations entre résidence actuelle et résidence antérieure au niveau régional, et ventilés par groupe socio-professione
#' flux_migres_REG_CS1 <-
#' calcul_flux_migres(TABLE =FD_MIGCOM_2015 ,
#'                  VAR_NB = "IPONDI",
#'                  VAR_VENTIL = "CS1",
#'                  MIG_NET_INTERNE=TRUE,
#'                  NIVGEO_ANTE ="REG_ANTE",
#'                  NIVGEO_ACTU ="REG_ACTU")
#'}
#' @export
#'


calcul_flux_migres <- function(TABLE,NIVGEO_ANTE, NIVGEO_ACTU,VAR_NB, VAR_VENTIL=NULL, MIG_NET_INTERNE=TRUE){

  # controle pour matrice de migrations nettes internes : mêmes modalités dans NIVGEO_ANTE et NIVGEO_ACTU
  if(MIG_NET_INTERNE == TRUE) {
    TABLE_ok <-
      TABLE %>%
      filter(!!sym(NIVGEO_ANTE) %in% c(TABLE %>% distinct(!!sym(NIVGEO_ACTU)) %>% pull()) &
               !!sym(NIVGEO_ACTU) %in% c(TABLE %>% distinct(!!sym(NIVGEO_ANTE)) %>% pull() ) )
  }

  # si variable de ventilation renseignée -> groupement par NIVGEO et VAR_VENTIL
  else {
    TABLE_ok <-  TABLE
  }

  #  cas 1 : pas de variable de ventilation renseignée -> groupement par NIVGEO uniquement
  if(missing(VAR_VENTIL)) {

    # création de la table de comptage de flux
    NIVGEO_flux_mig <-
      TABLE_ok %>%
      group_by(!!sym(NIVGEO_ACTU),
               !!sym(NIVGEO_ANTE)) %>%
      summarise(nb_ind = sum(!!sym(VAR_NB))) %>%
      # pourcentage en ligne sur total NIVGEO_ACTU
      mutate(pct_ind_ACTU = nb_ind / sum(nb_ind)) %>%
      # pourcentage en ligne sur total NIVGEO_ANTE
      ungroup() %>%
      group_by(!!sym(NIVGEO_ANTE)) %>%
      mutate(pct_ind_ANTE = nb_ind / sum(nb_ind)) %>%
      # # type de cas de migration
      mutate(cas_MIG = case_when(!!sym(NIVGEO_ACTU) == !!sym(NIVGEO_ANTE) ~ "ISO",
                                 !!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ~ "MIG",
                                 TRUE ~ "NA") )

  }

  #  cas 2 : variable de ventilation renseignée -> groupement par NIVGEO et variable de ventilation
  else {

    # création de la table de comptage de flux
    NIVGEO_flux_mig <-
      TABLE_ok %>%
      group_by(!!sym(VAR_VENTIL),
               !!sym(NIVGEO_ACTU),
               !!sym(NIVGEO_ANTE)) %>%
      summarise(nb_ind = sum(!!sym(VAR_NB))) %>%
      # pourcentage en ligne sur total NIVGEO_ACTU
      mutate(pct_ind_RES = nb_ind / sum(nb_ind)) %>%
      # pourcentage en ligne sur total NIVGEO_ANTE
      ungroup() %>%
      group_by(!!sym(VAR_VENTIL),
               !!sym(NIVGEO_ANTE)) %>%
      mutate(pct_ind_DCRAN = nb_ind / sum(nb_ind)) %>%
      # # type de cas de migration
      mutate(cas_MIG = case_when(!!sym(NIVGEO_ACTU) == !!sym(NIVGEO_ANTE) ~ "ISO",
                                 !!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ~ "MIG",
                                 TRUE ~ "NA") )

  }

}


