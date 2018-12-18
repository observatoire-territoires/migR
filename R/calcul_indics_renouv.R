#' @name calcul_indics_renouv
#'
#' @title Calculer les indicateurs de catégorisation et de renouvellement de la population par les migrations
#'
#' @description A partir d'une table en entrée contenant les indicateurs relatifs aux migrations résidentielles par territoire (population entrante, sortante, autochtone et présente).
#'
#' @param TABLE Table en entrée contenant les indicateurs relatifs aux migrations résidentielles par territoire (population entrante, sortante, autochtone et présente).
#' @param NIVGEO Nom du champ de la table en entrée contenant le niveau géographique.
#' @param NB_ENTR Nom du champ de la table en entrée contenant le nombre d'individus étant entré dans le territoire.
#' @param NB_SORT Nom du champ de la table en entrée contenant le nombre d'individus étant sorti du territoire.
#' @param NB_AUTO Nom du champ de la table en entrée contenant le nombre d'individus étant présente dans le territoire en N-x.
#' @param NB_PRES Nom du champ de la table en entrée contenant le nombre d'individus étant présente dans le territoire en N.
#' @param VAR_VENTIL Nom du champ de la variable de ventilation de la population, de type tranche d'âge, groupe socio-professionnel, etc... (facultatif).
#'
#'
#' @return Renvoie une table contenant l'identifiant des territoires ainsi que les indicateurs par type de migrations (immigration, émigration, migrations) par modalité de la variable de ventilation (groupe socio-professionnel par exemple)
#'
#' @importFrom dplyr tribble distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_all bind_rows everything funs vars
#' @importFrom rlang sym
#' @importFrom tidyr gather
#'
#' @examples
#'\dontrun{
#' # Calcul les indicateurs de catégorisation et de renouvellement de la population par les migrations selon le groupe socio-professionnel au niveau régional
#'indics_mig_EPCI_CS1_RENOUV <-
#'  calcul_indics_renouv(TABLE = indics_mig_EPCI_CS1,
#'                       NIVGEO = "EPCI",
#'                       NB_ENTR = "nb_ind_ENTR",
#'                       NB_SORT = "nb_ind_SORT",
#'                       NB_AUTO = "nb_ind_AUTO",
#'                       NB_PRES = "nb_ind_PRES",
#'                       VAR_VENTIL ="CS1")
#'
#'}
#'
#'
#' @details
#' Les indicateurs générés dans le champ 'type_indice' de la table de sortie sont les suivants (cf. vignette pour les définitions exactes et les formules de calcul) :
#' \itemize{
#' \item{Indice de catégorisation par l'immigration ('ICI')}
#' \item{Indice de catégorisation par l'émigration ('ICE')}
#' \item{Indice de catégorisation par les migrations ('ICM')}
#' \item{Indice de renouvellement par l'immigration ('IRI')}
#' \item{Indice de renouvellement par l'émigration ('IRE')}
#' \item{Indice de renouvellement par les migrations ('IRM')}
#' \item{Evolution de la part de la catégorie au sein de la population par le jeu des migrations internes ('evol_pct_AUTO_PRES')}}
#'
#' Chacun des 3 indices de catégorisation (ICI, ICE, ICM) est calculé pour chacune des modalités de la variable de ventilation indiquées dans le champ qui porte son nom.
#' Les 3 indices de renouvellement (IRI, IRE, IRM) ne sont au contraire indiqués qu'une seule fois.
#' La valeur de l'indice correspondant est indiquée dans le champ 'valeur'.
#' L'évolution de la part de la catégorie au sein de la population par le jeu des migrations internes (evol_pct_AUTO_PRES) est égale à la différence entre la part de cette catégorie dans la population présente et la part de cette même catégorie dans la population autochtone.
#'
#' @export
#'


calcul_indics_renouv <- function(TABLE, NIVGEO, NB_ENTR, NB_SORT, NB_AUTO,NB_PRES, VAR_VENTIL) {

  # pré-traitements
  indics_mig_RENOUV <-
    TABLE %>%
    select(!!sym(NIVGEO),!!sym(VAR_VENTIL),  !!sym(NB_ENTR), !!sym(NB_SORT), !!sym(NB_AUTO), !!sym(NB_PRES)) %>%
    # calcul part de chaque CS dans la pop entrante, sortante, autochtone
    group_by(!!sym(NIVGEO)) %>%
    mutate_at(.vars = vars( c(!!sym(NB_ENTR), !!sym(NB_SORT), !!sym(NB_AUTO)) ),
              .funs =  funs(pct = ./sum(.) ))  %>%
    # différences de taux
    mutate(diff_pct_AUTO_ENTR = (!!sym(paste0(NB_AUTO, "_pct")) - !!sym(paste0(NB_ENTR, "_pct"))) ^2 ,
           diff_pct_AUTO_SORT = (!!sym(paste0(NB_AUTO, "_pct")) - !!sym(paste0(NB_SORT, "_pct"))) ^2 ,
           SM = !!sym(NB_ENTR) - !!sym(NB_SORT),
           ratio_SM_ind_pres = (abs(SM) / !!sym(NB_PRES)) ^2,
           # évolution de la part de la CS par les migrations internes
           evol_pct_AUTO_PRES = !!sym(paste0(NB_PRES, "_pct")) - !!sym(paste0(NB_AUTO, "_pct")) )

  # indices de renouvellement global
  indics_mig_RENOUV.GLOBAL <-
    indics_mig_RENOUV %>% ungroup() %>%
    group_by(!!sym(NIVGEO)) %>%
    summarise(IRI = sum(diff_pct_AUTO_ENTR),
              IRE = sum(diff_pct_AUTO_SORT),
              IRM = sqrt(sum(ratio_SM_ind_pres))  ) %>%
    mutate(!!VAR_VENTIL := "GLOBAL") %>%
    select(NIVGEO,VAR_VENTIL,IRI,IRE,IRM ) %>%
    gather(type_indice, valeur, - NIVGEO,-VAR_VENTIL)



  # indices de renouvellement par catégorie de ventilation

  # indices de modifications structure population
  indics_mig_RENOUV.CLASSE <-
    indics_mig_RENOUV %>%
    mutate(ICI = (!!sym(paste0(NB_ENTR, "_pct")) - !!sym(paste0(NB_AUTO, "_pct"))),
           ICE = (!!sym(paste0(NB_AUTO, "_pct")) - !!sym(paste0(NB_SORT, "_pct")))) %>%
    # calcul des SM et nb_ind_PRES_pct sur tout sauf la modalité en question
    left_join(
      indics_mig_RENOUV %>% ungroup() %>%
        select(!!sym(NIVGEO),
               nb_ind_PRES_TOT = !!sym(NB_PRES),
               nb_ind_ENTR_TOT = !!sym(NB_ENTR),
               nb_ind_SORT_TOT = !!sym(NB_SORT)) %>%
        group_by(!!sym(NIVGEO)) %>%
        summarise_if(is.numeric, sum) %>%
        mutate(SM_TOT = nb_ind_ENTR_TOT - nb_ind_SORT_TOT),
      by = NIVGEO
    ) %>%
    mutate( ICM = ( SM / nb_ind_PRES ) - ( (SM_TOT - SM) / (nb_ind_PRES_TOT-!!sym(NB_PRES) ) ) ) %>%
    select(NIVGEO,VAR_VENTIL,ICI,ICE,ICM, evol_pct_AUTO_PRES ) %>%
    gather(type_indice, valeur, - NIVGEO, -VAR_VENTIL)

  indics_mig_RENOUV.OUT <- indics_mig_RENOUV.GLOBAL %>% bind_rows(indics_mig_RENOUV.CLASSE)

}



