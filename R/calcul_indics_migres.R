#' @name calcul_indics_migres
#'
#' @title Calculer les indicateurs relatifs aux migrations résidentielles pour chaque zone.
#'
#' @description A partir d'une table en entrée contenant un champ de résidence actuelle et un champ de résidence antérieure sur le même niveau géographique, une table en sortie est créée contenant les indicateurs relatifs aux migrations résidentielles (liste des indicateurs : cf. details ci-dessous).
#'
#' @param TABLE Table en entrée contenant a minima un champ de résidence actuelle et un champ de résidence antérieure sur le même niveau géographique, ainsi qu'un champ comptablisant le nombre d'individus correspondant.
#' @param NIVGEO_ANTE Nom du champ de la table en entrée contenant le niveau géographique de résidence antérieure.
#' @param NIVGEO_ACTU Nom du champ de la table en entrée contenant le niveau géographique de résidence actuelle.
#' @param NIVGEO Nom du champ de la table en sortie contenant le niveau géographique.
#' @param VAR_NB Nom du champ de la table en entrée contenant le nombre d'individus correspondant à la migration résidence antérieure / résidence actuelle.
#' @param VAR_VENTIL Nom du champ de la variable de ventilation de la population, de type tranche d'âge, groupe socio-professionnel, etc... (facultatif).
#'
#' @return Renvoie une table contenant un champ contenant l'identifiant des territoires ainsi que les indicateurs relatifs aux migrations résidentielles.
#'
#' @importFrom dplyr distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_if vars funs
#' @importFrom rlang sym
#'
#' @examples
#' # Calcul du nombre de migrations entre résidence actuelle et résidence antérieure au niveau départemental
#'indics_migres_REG <-
#'  calcul_indics_migres(TABLE =flux_migres_DEP,
#'                       NIVGEO_ANTE ="DEP_ANTE",
#'                       NIVGEO_ACTU ="DEP_ACTU",
#'                       NIVGEO ="DEP",
#'                       VAR_NB = "nb_ind")
#'
#' # Calcul du nombre de flux entre résidence actuelle et résidence antérieure au niveau régional, et ventilés par groupe socio-professionel
#' indics_migres_REG_CS1 <-
#'  calcul_indics_migres(TABLE =flux_migres_REG_CS1,
#'                       NIVGEO_ANTE ="DEP_ANTE",
#'                       NIVGEO_ACTU ="DEP_ACTU",
#'                       NIVGEO ="DEP",
#'                       VAR_NB = "nb_ind")
#'
#'
#' @details
#' Les indicateurs générés dans la table de sortie sont les suivants (cf. vignette pour les définitions exactes et les formules de calcul) : \cr
#' \itemize{
#' \item{Population présente ('nb_ind_PRES')}
#' \item{Population autochtone ('nb_ind_AUTO')}
#' \item{Population stable ('nb_ind_ISO')}
#' \item{Population entrante ('nb_ind_ENTR')}
#' \item{Population sortante ('nb_ind_SORT')}
#' \item{Solde migratoire ('SM')}
#' \item{Taux de migration nette interne ('TM')}
#' \item{Taux de rotation nette interne ('TR')}
#' \item{Part d'entrants ('PE')}
#' \item{Part de sortants ('PS')}}
#'
#' @export
#'
calcul_indics_migres <- function(TABLE, NIVGEO_ACTU, NIVGEO_ANTE,NIVGEO, VAR_NB,VAR_VENTIL=NULL) {

  #  cas 1 : pas de variable de ventilation renseignée -> groupement par NIVGEO uniquement

  if(missing(VAR_VENTIL)) {

    NIVGEO_indics_mig <-
      TABLE %>%
      # nombre d'iso
      filter(!!sym(NIVGEO_ACTU) == !!sym(NIVGEO_ANTE) ) %>%
      group_by(!!sym(NIVGEO_ANTE)) %>%
      summarise(nb_ind_ISO = sum(!!sym(VAR_NB))) %>%
      rename(NIVGEO = !!sym(NIVGEO_ANTE)) %>%
      #nombre d'entrées
      left_join(  TABLE %>%
                    filter(!!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ) %>%
                    group_by(!!sym(NIVGEO_ANTE)) %>%
                    summarise(nb_ind_SORT = sum(!!sym(VAR_NB))) %>%
                    rename(NIVGEO = !!sym(NIVGEO_ANTE)) ,
                  by = 'NIVGEO' ) %>%
      #   #nombre de sorties
      left_join(  TABLE %>%
                    filter(!!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ) %>%
                    group_by(!!sym(NIVGEO_ACTU)) %>%
                    summarise(nb_ind_ENTR = sum(!!sym(VAR_NB))) %>%
                    rename(NIVGEO = !!sym(NIVGEO_ACTU)) ,
                  by = 'NIVGEO' ) %>%
      #   # valeur à 0 si NA
      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
      #solde migratoire
      mutate(SM =nb_ind_ENTR - nb_ind_SORT ) %>%
      # population résidente
      mutate(nb_ind_PRES = nb_ind_ISO + nb_ind_ENTR) %>%
      # population autochtone
      mutate(nb_ind_AUTO = nb_ind_ISO + nb_ind_SORT) %>%
      # indicateurs relatifs
      mutate(TM = (nb_ind_ENTR - nb_ind_SORT) / ((nb_ind_AUTO + nb_ind_PRES)/2) ,
             PS = nb_ind_SORT /  nb_ind_AUTO,
             PE = nb_ind_ENTR / nb_ind_PRES,
             TR = (nb_ind_ENTR + nb_ind_SORT) / ((nb_ind_AUTO + nb_ind_PRES)/2)) %>%

      # renommer variable NIVGEO
      rename(!!NIVGEO := NIVGEO)

  }

  #  cas 2 : variable de ventilation renseignée -> groupement par NIVGEO et variable de ventilation

  else {

    NIVGEO_indics_mig <-
      TABLE %>%
      # nombre d'iso
      filter(!!sym(NIVGEO_ACTU) == !!sym(NIVGEO_ANTE) ) %>%
      group_by(!!sym(NIVGEO_ANTE),!!sym(VAR_VENTIL)) %>%
      summarise(nb_ind_ISO = sum(!!sym(VAR_NB))) %>%
      rename(NIVGEO = !!sym(NIVGEO_ANTE))  %>%
      # #nombre d'entrées
      left_join(  TABLE %>%
                    filter(!!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ) %>%
                    group_by(!!sym(NIVGEO_ANTE),!!sym(VAR_VENTIL)) %>%
                    summarise(nb_ind_SORT = sum(!!sym(VAR_NB))) %>%
                    rename(NIVGEO = !!sym(NIVGEO_ANTE)) ,
                  by = c('NIVGEO',VAR_VENTIL ) ) %>%
      # #nombre de sorties
      left_join(  TABLE %>%
                    filter(!!sym(NIVGEO_ACTU) != !!sym(NIVGEO_ANTE) ) %>%
                    group_by(!!sym(NIVGEO_ACTU),!!sym(VAR_VENTIL)) %>%
                    summarise(nb_ind_ENTR = sum(!!sym(VAR_NB))) %>%
                    rename(NIVGEO = !!sym(NIVGEO_ACTU)) ,
                  by = c('NIVGEO',VAR_VENTIL ) ) %>%
      # valeur à 0 si NA
      mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>%
      #solde migratoire
      mutate(SM =nb_ind_ENTR - nb_ind_SORT ) %>%
      # # population résidente
      mutate(nb_ind_PRES = nb_ind_ISO + nb_ind_ENTR) %>%
      # population autochtone
      mutate(nb_ind_AUTO = nb_ind_ISO + nb_ind_SORT) %>%
      # indicateurs relatifs
      mutate(TM = (nb_ind_ENTR - nb_ind_SORT) / ((nb_ind_AUTO + nb_ind_PRES)/2) ,
             PS = nb_ind_SORT /  nb_ind_AUTO,
             PE = nb_ind_ENTR / nb_ind_PRES,
             TR = (nb_ind_ENTR + nb_ind_SORT) / ((nb_ind_AUTO + nb_ind_PRES)/2))

    # renommer variable NIVGEO
    NIVGEO_indics_mig <- NIVGEO_indics_mig %>% rename(!!NIVGEO := NIVGEO)

  }
}

