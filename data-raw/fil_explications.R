
Principales fonctionnalités du package MOBIRESID

# Charger le fichier détail 'Migrations résidentielles' du RP Insee

# le télécharger depuis le site de l'Insee, le stocker dans un dossier en local puis le charger dans votre session R
FD_MIGCOM_2014 <- charger_FD_MIGCOM(anneeRP = "2014",  telechargement = TRUE, dossier_dest_TL = "./data")
Le fichier détail "Migrations résidentielles" indique pour l'ensemble des résidents en France (une ligne par individu, chacun ayant un poids de pondération)
sa commune de résidence actuelle, sa commune de résidence antérieure, ainsi qu'une vingtaine de variables précisant les caractéristiques socio-démographiques de cet
individu (sexe, age, groupe socio-professionnel, etc...)

Le fichier détail "Migrations résidentielles" est disponible pour le RP 2014 (résidence antérieure 1 an auparavant), le RP 2013 (idem), et le RP 2008 (résidence antérieure 5 ans auparavant),

Attention les fichiers sont volumineux, vérifier que votre connexion internet est d'attaque.
Si vous avez déjà le fichier détail stocké en local, il suffit alors d'indiquer son chemin pour le charger en session.
FD_MIGCOM_2014 <- charger_FD_MIGCOM(chemin_FD = "C:/Users/mgarnier/Desktop/sauvegarde_temporaire/RP/FD_MIGCOM_2014.txt")

# Ajouter des niveaux géographiques supra-communaux

Pour jouer nos analyses à différentes échelles et optimiser nos temps de traitements par la suite, on ajoute dans le fichier détail "Migrations résidentielles" des mailles supra-communales à partir des
champs indiquant la commune de résidence actuelle (champ 'COMMUNE') et la commune de résidence antérieure (champ 'DCRAN').

Ici, on précise que les niveaux supra-communaux ajoutés seront le département (code court 'DEP'), l'intercommunalité ('EPCI') et le type de densité d'après la grille communale de densité de l'Insee (TYPEDENS).
La liste des niveaux supra-communaux disponibles est précisé dans la documentation de la fonction ajout_NIVGEO_SUPRACOMM.
Le fichier détail utilisé ici étant issu du RP 2014 de l'Insee, les codes communes sont en géographie N+2 soit dans le Code Officiel Géographique 2016.
On précise également que les niveaux supra-communaux ajoutés seront en COG 2018, et que les champs nouvellement crées seront suffixés avec le terme 'ANTE' pour les niveaux supracommunaux relatifs à la résidence antérieure
et 'ACTU' pour la résidence actuelle.


FD_MIGCOM_2014  <-
ajout_NIVGEO_SUPRACOMM(TABLE = FD_MIGCOM_2014 ,
                      CODE_COMMUNE = "DCRAN",
                      SUFFIXE = "ANTE",
                      NIVGEO= c("DEP",'EPCI','TYPEDENS'),
                      COG_IN = 2016, COG_NIVGEO = 2018) %>%
ajout_NIVGEO_SUPRACOMM(TABLE = . ,
                      CODE_COMMUNE = "COMMUNE",
                      SUFFIXE = "ACTU",
                      NIVGEO= c("DEP",'EPCI','TYPEDENS'),
                      COG_IN = 2016, COG_NIVGEO = 2018)


# Calculer le volume de migrations résidentielles entre zones

A partir du fichier détail (une ligne pour chaque individu), on souhaite connaitre le nombre d'individus ayant migré d'un territoire (résidence antérieure) à un autre territoire (résidence actuelle).
#La fonction calcul_flux_migres génère une table contenant le niveau géographique antérieur, le niveau géographique actuel ainsi que le nombre d'individus ayant migré de l'un à l'autre.

#Si une variable de ventilation de la population est précisée (sexe, tranche d'âge, groupe socio-professionnel, etc...), un nouveau champ précisant cette information sera ajoutée à la table en sortie

flux_migres_DEP <-
calcul_flux_migres(TABLE =FD_MIGCOM_2014 ,
                 VAR_NB = "IPONDI",
                 MIG_NET_INTERNE=TRUE,
                 NIVGEO_ANTE ="DEP_ANTE",
                 NIVGEO_ACTU ="DEP_ACTU")

#pour calculer le nombre d'individus de chaque groupe socio-professionnel ayant migré entre régions, on pourra executer ce qui suit :
flux_migres_REG_CS1 <-
  calcul_flux_migres(TABLE =FD_MIGCOM_2014 ,
                     VAR_NB = "IPONDI",
                     MIG_NET_INTERNE=TRUE,
                     VAR_VENTIL = "CS1",
                     NIVGEO_ANTE ="REG_ANTE",
                     NIVGEO_ACTU ="REG_ACTU")


# Calculer les indicateurs relatifs aux migrations pour chaque territoire

A partir d'une table de comptage de migrations entre territoires de résidence antérieure et territoires de résidence actuelle, la fonction calcul_indics_migres
génère une nouvelle table contenant les indicateurs relatifs à chaque territoire :
#' \item{Population présente ('nb_ind_PRES')}
#' \item{Population autochtone ('nb_ind_AUTO')}
#' \item{Population stable ('nb_ind_ISO')}
#' \item{Population entrante ('nb_ind_ENTR')}
#' \item{Population sortante ('nb_ind_SORT')}
#' \item{Solde migratoire ('SM')}
#' \item{Taux de migration nette interne ('TM')}
#' \item{Taux de rotation nette interne ('TR')}
#' \item{Part d'entrants ('PE')}
#' \item{Part de sortants('PS')}

#Si une variable de ventilation de la population est précisée (sexe, tranche d'âge, groupe socio-professionnel, etc...), les indicateurs seront calculés pour chacun des groupes de population dans chaque territoire.


indics_migres_DEP <-
  calcul_indics_migres(TABLE =flux_migres_DEP,
                       VAR_NB = "nb_ind",
                       #VAR_VENTIL = "CS1",
                       NIVGEO ="DEP",
                       NIVGEO_ACTU ="DEP_ACTU",
                       NIVGEO_ANTE ="DEP_ANTE")


indics_migres_REG_CS1 <-
  calcul_indics_migres(TABLE =flux_migres_REG_CS1,
                       VAR_NB = "nb_ind",
                       VAR_VENTIL = "CS1",
                       NIVGEO ="REG",
                       NIVGEO_ACTU ="REG_ACTU",
                       NIVGEO_ANTE ="REG_ANTE")


### ajout libelles NIVGEO


pour rendre les tables plus facilement intelligible, la fonction ajout_libelles_NIVGEO ajoute le libellé des zones géographiques (communales ou supra-communales) à partir de leur code.

On peut ainsi créer un nouveau champ contenant le nom des régions administratives à partir de leur code.
Le millésime du COG à indiquer est celui d

indics_migres_REG_CS1 <-
ajout_libelles_NIVGEO(TABLE = indics_migres_REG_CS1,
                      NIVGEO_IN ="REG",
                      COG_NIVGEO = 2018)

flux_migres_EPCI <-
 ajout_libelles_NIVGEO(TABLE = flux_migres_EPCI,
                       NIVGEO_IN ="EPCI_ANTE",
                       NIVGEO_OUT ="EPCI",
                       LIBGEO_OUT = "LIB_EPCI_ANTE",
                       COG_NIVGEO = 2018) %>%
  ajout_libelles_NIVGEO(TABLE = .,
                        NIVGEO_IN ="EPCI_ACTU",
                        NIVGEO_OUT ="EPCI",
                        LIBGEO = "LIB_EPCI_ACTU",
                        COG_NIVGEO = 2018)


## ajout libellés VAR_VENTIL


indics_migres_REG_CS1 <-
  ajout_libelles_VARVENTIL_INSEE(TABLE = indics_migres_REG_CS1,
                         VAR ="CS1",
                         MILLESIME_RP = 2014)


 # Calcul du nombre de migrations entre résidence actuelle et résidence antérieure au niveau départemental
indics_migres_REG_CS1_RENOUV <-
  calcul_indics_renouv(TABLE = indics_migres_REG_CS1,
                       NIVGEO = "REG",
                       NB_ENTR = "nb_ind_ENTR",
                       NB_SORT = "nb_ind_SORT",
                       NB_AUTO = "nb_ind_AUTO",
                       NB_PRES = "nb_ind_PRES",
                       VAR_VENTIL ="CS1")

