
# bible : https://github.com/ColinFay/erum2018/

##########
# packages dependances
library(data.table)
library(dplyr)
library(tidyr)
library(COGugaison)
library(janitor)
library(magrittr)
library(stringr)

memory.limit(size = 8000000)

############
# référentiels sources à ajouter

library(readxl)
library(janitor)
# import grille densité et ZAU
COMM_GRIDENS <- read_excel("N:/Transverse/Donnees_Obs/Donnees_Statistiques/Insee/Grille de densité communale/2016/grille_densite_2016/grille_densite_2016.xls", sheet = "grille_densite_2016",col_types = "text") %>%
  clean_names() %>%
  mutate(CODGEO = str_sub(paste0("00",depcom),-5,-1)) %>%
  dplyr::select(CODGEO, TYPEDENS = typo_degre_de_densite)

REF_VARS_INSEE_FD_MIGCOM <- read_excel("N:/Transverse/Donnees_Obs/Donnees_Statistiques/Insee/RP/RP2008/Fichiers détails/RP2008_migcom_txt/REF_VARS_MIGCOM_RP2008.xlsx", sheet = "data") %>%
  rbind(read_excel("N:/Transverse/Donnees_Obs/Donnees_Statistiques/Insee/RP/RP2014/Fichiers détails/rp2014_MIGCOM_txt/REF_VARS_MIGCOM_RP2014.xlsx", sheet = "data"))

# modification de la modalité CS1 artisans

REF_VARS_INSEE_FD_MIGCOM <- REF_VARS_INSEE_FD_MIGCOM %>%
  mutate(LIBMOD = case_when(CODVAR %in% 'CS1' & CODMOD %in% '2' ~ "Artisans, commerçants et chefs d'entreprise",
                            TRUE ~ as.character(LIBMOD)))

# sauvegarde des données dans /data

save(COMM_GRIDENS, file = "./data/COMM_GRIDENS.RData")
save(REF_VARS_INSEE_FD_MIGCOM, file = "./data/REF_VARS_INSEE_FD_MIGCOM.RData")


#####################
# données et environnement de test
#options(encoding = 'UTF-8')
options(scipen=999)


# test fonctions
FD_MIGCOM_2014 <- chargement_fd_migcom(chemin_FD = "C:/Users/mgarnier/Desktop/sauvegarde_temporaire/RP/FD_MIGCOM_2014.txt")
#FD_MIGCOM_2014 <- chargement_fd_migcom(anneeRP = "2014",  telechargement = TRUE, dossier_dest_TL = "./datatest")

FD_MIGCOM_2014  <-
ajout_nivgeo_supracomm(TABLE = FD_MIGCOM_2014 ,
                       CODE_COMMUNE = "DCRAN",
                       SUFFIXE = "ANTE",
                       NIVGEO= c("DEP",'REG'),
                       COG_IN = 2016, COG_NIVGEO = 2018) %>%
ajout_nivgeo_supracomm(TABLE = . ,
                       CODE_COMMUNE = "COMMUNE",
                       SUFFIXE = "ACTU",
                       NIVGEO= c("DEP",'REG'),
                       COG_IN = 2016, COG_NIVGEO = 2018)


flux_migres_REG_CS1 <-
calcul_flux_migres(TABLE =FD_MIGCOM_2014 ,
                 VAR_NB = "IPONDI",
                 VAR_VENTIL = "CS1",
                 MIG_NET_INTERNE=TRUE,
                 NIVGEO_ANTE ="REG_ANTE",
                 NIVGEO_ACTU ="REG_ACTU")


flux_migres_REG_CS1 <-
 ajout_libelles_nivgeo(TABLE = flux_migres_REG_CS1,
                       NIVGEO_IN ="REG_ANTE",
                       NIVGEO_OUT ="REG",
                       LIBGEO_OUT = "LIB_REG_ANTE",
                       COG_NIVGEO = 2018) %>%
  ajout_libelles_nivgeo(TABLE = .,
                        NIVGEO_IN ="REG_ACTU",
                        NIVGEO_OUT ="REG",
                        LIBGEO = "LIB_REG_ACTU",
                        COG_NIVGEO = 2018)


ajout_libelles_varventil_insee(TABLE = flux_migres_REG_CS1,
                              VAR ="CS1",
                              MILLESIME_RP = 2014)

indics_migres_REG_CS1 <-
 calcul_indics_migres(TABLE =flux_migres_REG_CS1,
                      NIVGEO_ANTE ="REG_ANTE",
                      NIVGEO_ACTU ="REG_ACTU",
                      NIVGEO ="REG",
                      VAR_VENTIL = "CS1",
                      VAR_NB = "nb_ind")

indics_migres_EPCI_CS1_RENOUV <-
  calcul_indics_renouv(TABLE = indics_migres_REG_CS1,
                       NIVGEO = "REG",
                       NB_ENTR = "nb_ind_ENTR",
                       NB_SORT = "nb_ind_SORT",
                       NB_AUTO = "nb_ind_AUTO",
                       NB_PRES = "nb_ind_PRES",
                       VAR_VENTIL ="CS1")

