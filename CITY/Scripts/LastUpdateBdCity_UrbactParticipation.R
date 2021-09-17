
##==========================================================================##         
#                                 BD city - Update with complete urbact date
#                          
# DESCRIPTION : mise à jour de la DB city, remplacer le nombre de participations Urbact, grâce à la table complete 
#                           (1500 participations au lieu de 700 lorsque la table ne comportait que 2 phases du programme)
#
# PG, AD, mars 2020
##==========================================================================##         


# library

library(tidyverse)
library(tidylog)
library(skimr)
library(sf)


DBCity_LauUmzFua <- readRDS("~/BD_Keep_Interreg/CITY/Data/DBCity_LauUmzFua.rds")

DBCity <- readRDS("~/BD_Keep_Interreg/CITY/Data/DBCity.rds")

UrbactParticipation <- read.csv2("KEEP/AD/URBACT/UrbactDB_Vfinal/URBACT_Membership_GNidCorr_complete_V2.csv")

# Participation Urbact by GN id 

GnURBACTpart <- UrbactParticipation %>% group_by(geonameId) %>% summarise(Nurbact = n())


#Join on db city


DBCity <- DBCity %>% left_join(GnURBACTpart)

DBCity <- DBCity %>% mutate(Nurbact = replace_na(Nurbact,0))

DBCity <- DBCity %>% 
  select(-members_urbact) %>% 
  rename(members_urbact = Nurbact) %>% 
  relocate(members_urbact, .after = members_etmun)



saveRDS(DBCity, "CITY/Data/DBCity.rds")


DBCity_LauUmzFua <- DBCity_LauUmzFua %>% left_join(GnURBACTpart)

DBCity_LauUmzFua <- DBCity_LauUmzFua %>% mutate(Nurbact = replace_na(Nurbact,0))

DBCity_LauUmzFua <- DBCity_LauUmzFua %>% 
  select(-members_urbact) %>% 
  rename(members_urbact = Nurbact) %>% 
  relocate(members_urbact, .after = members_etmun)


saveRDS(DBCity_LauUmzFua, "CITY/Data/DBCity_LauUmzFua.rds")
