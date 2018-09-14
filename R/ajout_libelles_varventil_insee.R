#' @name ajout_libelles_varventil_insee
#'
#' @title Ajouter le libellé d'une modalité de variable de ventilation
#'
#' @description Ajouter le libellé d'une modalité de variable de ventilation (issue d'un fichier du RP Insee) à partir du champ contenant le code en format court.
#'
#' @param TABLE Table en entrée avec un champ contenant les modalités d'une variable de ventilation en format court (liste : cf. details ci-dessous).
#' @param VAR Nom du champ contenant les modalités d'une variable de ventilation en format court (liste : cf. details ci-dessous).
#' @param MILLESIME_RP Millésime du recensement RP de l'Insee de la variable de ventilation en entrée (liste : cf. details ci-dessous).
#'
#' @return Renvoie une table avec un champ supplémentaire contenant le libellé de la modalité.
#'
#' @importFrom dplyr tribble distinct pull mutate select left_join case_when group_by summarise ungroup rename mutate_all bind_rows everything
#' @importFrom rlang sym
#'
#' @examples
#' # Ajout du champ 'LIB_CS1' contentant le libellé en clair du code CS1 (aka "groupe socio-professionnel")
#' indics_mig_DEP_CS1 <-
#' ajout_libelles_varventil_insee(TABLE = indics_mig_DEP_CS1,
#'                                VAR ="CS1",
#'                                MILLESIME_RP = 2015)
#'
#'
#'
#' @details
#' Les millésimes du RP disponibles sont les suivants : 2008, 2013, 2014, 2015. \cr
#'
#' Les variables de ventilation disponbibles sont issues de la documentation des fichiers détail 'Migrations Résidentielles' et sont les suivants :
#' \itemize{
#' \item{Période regroupée d'achèvement de la construction de la maison ou de l'immeuble ('ACHLR')}
#' \item{Âge regroupé de la personne de référence du ménage en 8 classes d'âge ('AGEMEN8')}
#' \item{Âge quinquennal en années révolues ('AGEREVQ')}
#' \item{Ancienneté d'emménagement ('ANEMC')}
#' \item{Catégorie de population condensée ('CATPC')}
#' \item{Catégorie socioprofessionnelle en 8 postes ('CS1')}
#' \item{Catégorie socioprofessionnelle de la personne de référence du ménage en 8 postes ('CSM')}
#' \item{Diplôme le plus élevé ('DIPL_15')}
#' \item{Département de naissance (si né en France) ('DNAI')}
#' \item{Condition d'emploi ('EMPL')}
#' \item{Indicateur du lieu de naissance ('INAI')}
#' \item{Indicateur de nationalité condensé (Français/Étranger) ('INATC')}
#' \item{Indicateur de résidence antérieure au 1er janvier de l'année précédente ('IRAN')}
#' \item{Indicateur urbain du lieu de résidence antérieure au 1er janvier de l'année précédente ('IRANUU')}
#' \item{Lien à la personne de référence du ménage ('LPRM')}
#' \item{Indicateur Métropole ou DOM du lieu de résidence ('METRODOM')}
#' \item{Mode de cohabitation ('MOCO')}
#' \item{Activité économique en 17 postes (NA - A17) ('NA17')}
#' \item{Activité économique regroupée en 5 postes ('NA5')}
#' \item{Nombre de personnes du ménage (regroupé) ('NPERR')}
#' \item{Ancienneté de recherche d'emploi ('RECH')}
#' \item{Sexe ('SEXE')}
#' \item{Statut d'occupation détaillé du logement ('STOCD')}
#' \item{Type d'activité ('TACT')}
#' \item{Type d'activité de la personne de référence du ménage ('TACTM')}
#' \item{Mode de transport principal le plus souvent utilisé pour aller travailler ('TRANS')}
#' \item{Type de construction ('TYPC')}
#' \item{Type de logement ('TYPL')}
#' \item{Type de ménage regroupé (en 9 postes) ('TYPMR')}}
#'
#' @export
#'


ajout_libelles_varventil_insee <- function(TABLE, VAR, MILLESIME_RP) {

  ref_var <-
    REF_VARS_INSEE_FD_MIGCOM %>%
    filter(millesime_FD == !!MILLESIME_RP) %>%
    filter(CODVAR %in% !!VAR) %>%
    mutate_all(as.character) %>%
    select(CODMOD, LIBMOD)

  # affectation du libellé à la table
  TABLE <- TABLE %>% rename(VAR = VAR) %>%
    left_join(ref_var ,  by = c('VAR' = 'CODMOD')  ) %>%
    #ordre variables dans dataframe
    dplyr::select(VAR, LIBMOD, everything())

  # renommer variables NIVGEO et LIBGEO
  TABLE <- TABLE %>% rename(!!VAR := VAR, !!paste0(VAR, "_LIB") := LIBMOD )

}


