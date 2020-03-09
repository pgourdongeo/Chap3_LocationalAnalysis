###############################################################################
#                                GN key correct on all db
#                          
# DESCRIPTION : Correction des 3 bases avec les identifiants Geonames aggrégés à l'entité
#               LAU 
#
# PG, février 2020
##############################################################################

# Working directory huma-num
setwd("~/BD_Keep_Interreg/")

setwd("~/git/Chap3_LocationalAnalysis")

# library
library(tidylog)
library(tidyverse)
library(sf)



# Load DB to correct
urbactParticipation <- readRDS("KEEP/AD/URBACT/URBACT_Membership_GNid.rds")
urbactPartners <- readRDS("KEEP/AD/URBACT/URBACT_GNall_uniqueCity.rds") #Useless

etmunMembership <- readRDS("ETMUN/Data/ETMUN_Membership_GNid.rds")
etmunSnap <- readRDS("ETMUN/Data/sfETMUN_snap.RDS") # No GN
etmunPartner <- readRDS("ETMUN/Data/ETMUN_GNall_UniqueCity.rds") # Useless

eucicopSnap <- readRDS("KEEP/Data/sfParticipations_snap.RDS")
eucicopParticipation <- readRDS("KEEP/Data/Participations_All_Eucicop.RDS") #No GN
eucicopProjects <- readRDS("KEEP/Data/ProjectsEucicop_all_noduplicated.RDS")#No GN
eucicopPartners <-readRDS("KEEP/Data/UniquePartners_GNid_Eucicop.RDS")


## Load GN all db correct (dico and GN db)

DicoGN <- readRDS("CITY/Data/DicoAggrGN.rds")
DicoGN <- DicoGN %>% rename(geonameId = OldGNid, NewGeonameId = geonameId)
GNinfoAllDB <- readRDS("CITY/Data/UniqueGN_info_AllDB_Corr.rds")

GNinfoSimpleDb <- GNinfoAllDB %>% select(geonameId, asciiName, lng_GN, lat_GN)
skim(GNinfoSimpleDb)
### Fonction to change GN id in DB and join new info (lng lat  asciiName)

CorrectDb <- function(DftoCorrect, Dico, GNinfo){
  
  DftoCorrect <- DftoCorrect %>% left_join(Dico)%>% 
    mutate(geonameId = ifelse(is.na(NewGeonameId),geonameId, NewGeonameId))
  
  DftoCorrect <- DftoCorrect %>% select(-NewGeonameId, -asciiName, -lng_GN, - lat_GN)
  DftoCorrect <- DftoCorrect %>% left_join(GNinfo)
  
  return(DftoCorrect)
}


### Correction and replacement
# Urbact
urbactParticipation <- CorrectDb(DftoCorrect = urbactParticipation, Dico= DicoGN, GNinfo = GNinfoSimpleDb)

saveRDS(urbactParticipation, "CITY/CorrectedDB//URBACT_Membership_GNidCorr.RDS")
write.csv2(urbactParticipation, "CITY/CorrectedDB/URBACT_Membership_GNidCorr.csv", row.names = F)


# Etmun
etmunMembership  <- CorrectDb(DftoCorrect =  etmunMembership, Dico= DicoGN, GNinfo = GNinfoSimpleDb)

saveRDS(etmunMembership , "CITY/CorrectedDB/ETMUN_Membership_GNidCorr.RDS")
write.csv2(etmunMembership , "CITY/CorrectedDB/ETMUN_Membership_GNidCorr.csv",row.names = F)
# Eucicop
eucicopPartners  <- CorrectDb(DftoCorrect = eucicopPartners, Dico= DicoGN, GNinfo = GNinfoSimpleDb)

saveRDS( eucicopPartners  , "CITY/CorrectedDB/UniquePartners_GNid_EucicopCorr.RDS")
write.csv2( eucicopPartners  , "CITY/CorrectedDB/UniquePartners_GNid_EucicopCorr.csv", row.names = F)

