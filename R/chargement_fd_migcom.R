#' @name chargement_fd_migcom
#'
#' @title Charger un fichier détail MIGCOM de l'Insee
#'
#' @description Télécharger un fichier détail MIGCOM (Migrations résidentielles entre commune de résidence et commune de résidence antérieure) depuis le site de l'Insee puis le monter en mémoire, ou uniquement le charger si le fichier txt est disponible en local.
#'
#' @param telechargement Vaut TRUE si le fichier doit être téléchargé depuis le site internet de l'Insee, FALSE si le fichier txt est disponible en local.
#' @param monet Vaut TRUE si le fichier détail doit être importé dans une base de données `MonetDBLite`, FALSE s'il doit être importé en mémoire vive.
#' @param anneeRP Millésime du recensement de la population (RP) de l'Insee. Peut prendre les valeurs "2008", "2013", "2014" ou "2015". Non renseigné si telechargement vaut TRUE.
#' @param dossier_dest_TL Dossier de destination où va être stockée l'archive zip et les fichiers txt dézippés. S'il n'existe pas, il sera créé. Non renseigné si telechargement vaut FALSE.
#' @param chemin_FD Lien vers le fichier txt (FD_MIGCOM) en local. Non renseigné si telechargement vaut TRUE.
#'
#' @return Retourne une table de détail des migrations résidentielles avec pour chaque individu sa commune de résidence actuelle et commune de résidence antérieure.
#'
#' @importFrom dplyr %>% filter mutate
#' @importFrom utils download.file unzip
#' @importFrom readr read_csv
#' @importFrom tibble tribble as_tibble
#' @importFrom Insee2MonetDB Insee2MonetDB
#' @importFrom stringr str_replace
#'
#' @examples
#' # Téléchargement du fichier MIGCOM du dernier RP en date depuis le site internet de l'Insee dans le dossier "./data"
#' FD_MIGCOM_2015 <- chargement_fd_migcom(anneeRP = "2015",  telechargement = TRUE, dossier_dest_TL = "./data")
#'
#' # Chargement du fichier txt MIGCOM disponible en local
#' FD_MIGCOM_2015 <- chargement_fd_migcom(chemin_FD = "./data/rp2015_migcom_txt/FD_MIGCOM_2015.txt")
#' @export


chargement_fd_migcom <- function(telechargement = FALSE, monet = TRUE, anneeRP, dossier_dest_TL, chemin_FD){

  if(telechargement %in% TRUE){

    # création du dossier de destination
    if(missing(dossier_dest_TL) %in% FALSE){
      dir.create(file.path(dossier_dest_TL), showWarnings = FALSE)
    }

    # liste des fichiers MIGCOM disponibles
    liste_FD_MIGCOM <-
      tibble::tribble(
        ~millesime_RP, ~url_page, ~url_doc, ~url_txt, ~nom_txt,
        "2015","https://www.insee.fr/fr/statistiques/3566042?sommaire=3558417", "https://www.insee.fr/fr/statistiques/fichier/3566042/contenu_rp2015_migcom.pdf","https://www.insee.fr/fr/statistiques/fichier/3566042/rp2015_migcom_txt.zip","rp2015_migcom_txt",
        "2014","https://www.insee.fr/fr/statistiques/2866333?sommaire=2866354", "https://www.insee.fr/fr/statistiques/fichier/2866333/contenu_rp2014_migcom.pdf","https://www.insee.fr/fr/statistiques/fichier/2866333/rp2014_migcom_txt.zip","rp2014_migcom_txt",
        "2013","https://www.insee.fr/fr/statistiques/2409519?sommaire=2409559", "https://www.insee.fr/fr/statistiques/fichier/2409519/contenu_rp2013_migcom.pdf","https://www.insee.fr/fr/statistiques/fichier/2409519/rp2013_migcom_txt.zip","rp2013_migcom_txt",
        "2008","https://www.insee.fr/fr/statistiques/2408687?sommaire=2409062", "https://www.insee.fr/fr/statistiques/fichier/2408687/contenu_rp2008_migcom.pdf","https://www.insee.fr/fr/statistiques/fichier/2408687/rp2008_migcom_txt.zip","rp2008_migcom_txt")

    parametres_DL <- liste_FD_MIGCOM %>% filter(millesime_RP == anneeRP)
    # téléchargement du fichier zippé source
    download.file(parametres_DL$url_txt,
                  destfile = paste0(dossier_dest_TL, "/",parametres_DL$nom_txt, ".zip"),
                  method='auto')

    # dézipper fichiers
    unzip(paste0(dossier_dest_TL, "/",parametres_DL$nom_txt, ".zip"),
          exdir=paste0(dossier_dest_TL, "/", parametres_DL$nom_txt) )

    if (monet) {
      output <- Insee2MonetDB::Insee2MonetDB(csvfile = paste0(dossier_dest_TL, "/",parametres_DL$nom_txt,"/FD_MIGCOM_",anneeRP,".txt"))
      return(output %>% as.data.frame())
    }


    # lecture et import du fichier FD zippé
    output <- readr::read_delim(paste0(dossier_dest_TL, "/",parametres_DL$nom_txt,"/FD_MIGCOM_",anneeRP,".txt"),
                              delim = ";", col_types = cols(.default = "c")) %>%
      dplyr::mutate(IPONDI = stringr::str_replace(IPONDI, ",",'.')) %>%
      dplyr::mutate(AGEREVQ = as.numeric(AGEREVQ),
                    IPONDI = as.numeric(IPONDI)) %>%
      tibble::as_tibble()

  } else{

    if (monet) {
      output <- Insee2MonetDB::Insee2MonetDB(csvfile = chemin_FD)
      return(output %>% as.data.frame())
    }

    # lecture et import du fichier FD zippé
    output <- readr::read_delim(chemin_FD, delim = ";", col_types = cols(.default = "c")) %>%
      dplyr::mutate(IPONDI = stringr::str_replace(IPONDI, ",",'.')) %>%
      dplyr::mutate(AGEREVQ = as.numeric(AGEREVQ),
                    IPONDI = as.numeric(IPONDI)) %>%
      tibble::as_tibble()

  }


}
