load("./data/REF_VARS_INSEE_FD_MIGCOM.rdata")
View(REF_VARS_INSEE_FD_MIGCOM)

# vérifications que les libellés des modalités du nouveau millésime n'ont pas changé
# vs documentation fichier détail Insee

# si c'est ok, réplication des libellés 2015 pour 2016
library(tidyverse)
REF_VARS_INSEE_FD_MIGCOM <-
  REF_VARS_INSEE_FD_MIGCOM %>%
  rbind.data.frame(REF_VARS_INSEE_FD_MIGCOM %>%
                     filter(millesime_FD == 2015)%>%
                     mutate(millesime_FD = 2016))

# sauvegarde du nouveau référentiel en .rdata
save(REF_VARS_INSEE_FD_MIGCOM, file="./data/REF_VARS_INSEE_FD_MIGCOM.RData")
