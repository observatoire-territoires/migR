#' @name chargement_bd_histodemo
#'
#' @title Charger une base de données de séries historiques de population de l'Insee
#'
#' @description Télécharger la base de données "séries historiques" contenant les informations de démographique par période intercensitaire depuis 1968 depuis le site de l'Insee puis le monter en mémoire, ou uniquement la charger si le fichier xls est disponible en local.
#'
#' @param telechargement Vaut TRUE si le fichier doit être téléchargé depuis le site internet de l'Insee, FALSE si le fichier xls est disponible en local.
#' @param dossier_dest_TL Dossier de destination où va être stockée l'archive zip et le fichier xls dézippé. S'il n'existe pas, il sera créé. Non renseigné si telechargement vaut FALSE.
#' @param chemin_FD Lien vers le fichier xls (base-cc-serie-historique-AAAA.xls) en local. Non renseigné si telechargement vaut TRUE.
#'
#' @return Retourne une table communale contenant les informations de population/naissances/décès par période inter-censitaire depuis 1968.
#'
#' @importFrom dplyr %>% filter mutate
#' @importFrom utils download.file unzip
#' @importFrom readr read_csv
#' @importFrom tibble tribble as_tibble
#' @importFrom Insee2MonetDB Insee2MonetDB
#' @importFrom stringr str_replace
#' @importFrom readxl read_excel
#'
#' @examples
#' \dontrun{
#' # Téléchargement du fichier "séries historiques" du dernier RP en date depuis le site internet de l'Insee dans le dossier "./data"
#' COMM_HISTODEMO_2015 <- chargement_bd_histodemo(telechargement = TRUE, dossier_dest_TL = "./data")
#'
#' # Chargement du fichier xls "séries historiques" disponible en local
#' COMM_HISTODEMO_2015 <- chargement_bd_histodemo(chemin_FD = "./data/base-cc-serie-historique-2015/base-cc-serie-historique-2015.xls")
#' }
#'
#' @export


chargement_fd_migcom <- function(telechargement = FALSE, dossier_dest_TL, chemin_FD){

  # dernier millésime en date
  last_RP <- "2015"
  last_RP_ante <- "2010"

  if(telechargement %in% TRUE){
    # création du dossier de destination
    if(missing(dossier_dest_TL) %in% FALSE){
      dir.create(file.path(dossier_dest_TL), showWarnings = FALSE)
    }

    # dernier fichier "séries historiques" disponible
    parametres_DL <-
      tibble::tribble(
        ~millesime_RP, ~url_page, ~url_xls, ~nom_xls,
        last_RP,"https://www.insee.fr/fr/statistiques/3565661","https://www.insee.fr/fr/statistiques/fichier/3565661/base-cc-serie-historique-2015.zip", "base-cc-serie-historique-2015"
      )

    # téléchargement du fichier zippé source
    download.file(parametres_DL$url_xls,
                  destfile = paste0(dossier_dest_TL, "/",parametres_DL$nom_xls, ".zip"),
                  method='auto')

    # dézipper fichiers
    unzip(paste0(dossier_dest_TL, "/",parametres_DL$nom_xls, ".zip"),
          exdir=paste0(dossier_dest_TL, "/", parametres_DL$nom_xls) )


    # lecture et import du fichier zippé
    output <- readxl::read_excel(paste0(dossier_dest_TL, "/",parametres_DL$nom_xls,"/",parametres_DL$nom_xls,".xls"), sheet = "COM_2015",skip = 5) %>%
      rename(#paste0("POP_",last_RP) = sym(paste0( substr(last_RP,3,4), "_RP")),
        !!sym(paste0("POP_",last_RP)) := !!sym(paste0("P", substr(last_RP,3,4), "_POP")),
        !!sym(paste0("POP_",last_RP_ante)) := !!sym(paste0("P", substr(last_RP_ante,3,4), "_POP")),
        POP_1999 = D99_POP,
        POP_1990 = D90_POP,
        POP_1982 = D82_POP,
        POP_1975 = D75_POP,
        POP_1968 = D68_POP
      ) %>%
      select(-c(REG, DEP, LIBGEO))

  } else{


    # lecture et import du fichier zippé

    output <- readxl::read_excel(paste0(chemin_FD), sheet = "COM_2015",skip = 5) %>%
      rename(#paste0("POP_",last_RP) = sym(paste0( substr(last_RP,3,4), "_RP")),
        !!sym(paste0("POP_",last_RP)) := !!sym(paste0("P", substr(last_RP,3,4), "_POP")),
        !!sym(paste0("POP_",last_RP_ante)) := !!sym(paste0("P", substr(last_RP_ante,3,4), "_POP")),
        POP_1999 = D99_POP,
        POP_1990 = D90_POP,
        POP_1982 = D82_POP,
        POP_1975 = D75_POP,
        POP_1968 = D68_POP
      ) %>%
      select(-c(REG, DEP, LIBGEO))

  }

}
