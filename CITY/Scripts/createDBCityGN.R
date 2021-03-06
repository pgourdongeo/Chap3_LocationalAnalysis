###############################################################################
#                                 BD city
#                          
# DESCRIPTION : Création d'une base de données 'ville' par : 
#               1. compilation des trois bases ETMUN, KEEP-EUCICOP et 
#               URBACT selon les uniques geonameId -> BDCity_beforeCorr.RDS
#               2. aggrégation spatiale des geonameId situés dans une même commune
#               (ex Paris et ses arrondissements) - cf. AggregateGN_SpatialJoin.R
#               3. Refaire tourner ce script sur les fichiers corrigés -> en sortie,
#               BDCity.RDS
#
#
# PG, AD, février 2020
##############################################################################

# Working directory huma-num
#setwd("~/BD_Keep_Interreg")

setwd("~/git/Chap3_LocationalAnalysis")

# library
library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(skimr) 

# Load data - files before correction
# # Load DB
# urbact <- readRDS("KEEP/AD/URBACT/URBACT_Membership_GNid.RDS")
# etmun <- readRDS("ETMUN/Data/ETMUN_Membership_GNid.RDS")
# ## load eucicop
# partners <- readRDS("KEEP/Data/UniquePartners_GNid_Eucicop.RDS")
# participations <- readRDS("KEEP/Data/Participations_All_Eucicop.RDS")
# projects <- readRDS("KEEP/Data/ProjectsEucicop_all_noduplicated.RDS")

# Load data 
etmun <- readRDS("CITY/CorrectedDB/ETMUN_Membership_GNidCorr.RDS")
urbact <- readRDS("CITY/CorrectedDB/URBACT_Membership_GNidCorr.RDS")
partners <- readRDS("CITY/CorrectedDB/UniquePartners_GNid_EucicopCorr.RDS")
participations <- readRDS("KEEP/Data/Participations_All_Eucicop.RDS")
projects <- readRDS("KEEP/Data/ProjectsEucicop_all_noduplicated.RDS")



## Prepare eucicop participations
### Add coords partners to participations
eucicop <- participations %>% 
  left_join(partners, by = "ID_PARTNER")
### add 'Period' 
eucicop <- eucicop %>% 
  left_join(select(projects, Period, ID_PROJECT),  by = "ID_PROJECT")
### remove participations wo coords 
eucicop <- eucicop %>%
  filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

rm(participations, partners, projects)



# count participations/members in each GN city

## summarise members ETMUN by city
etmun <- etmun %>% 
  filter(!is.na(geonameId)) %>% 
  group_by(geonameId) %>% 
  mutate(members_etmun = sum(n())) 

## summarise members urbact by city
urbact <- urbact %>% 
  group_by(geonameId) %>% 
  mutate(members_urbact = sum(n()))

## summarise participations, partners and projects eucicop by city
eucicop <- eucicop %>% 
  group_by(geonameId) %>% 
  mutate(participations_eucicop = sum(n()),
         partners_eucicop = length(unique(ID_PARTNER)),
         projects_eucicop = length(unique(ID_PROJECT)))


# compile unique city of the three bases with the new var

## etmun
etmun_gnu <- etmun %>% 
  select(geonameId, asciiName, lat_GN, lng_GN, members_etmun) %>% 
  filter(!duplicated(geonameId))

## urbact
urbact_gnu <- urbact %>% 
  select(geonameId, asciiName, lat_GN, lng_GN, members_urbact) %>% 
  filter(!duplicated(geonameId)) %>%  
  mutate(lat_GN = as.numeric(lat_GN), 
         lng_GN = as.numeric(lng_GN))

## eucicop
eucicop_gnu <- eucicop %>% 
  select(geonameId, asciiName, lat_GN, lng_GN, 
         participations_eucicop, partners_eucicop, projects_eucicop) %>% 
  filter(!duplicated(geonameId)) 

## Compile
##
vec <- c(etmun_gnu$geonameId, urbact_gnu$geonameId, eucicop_gnu$geonameId)
allGNU <- data.frame(geonameId = unique(vec))
rm(vec)

. <- full_join(etmun_gnu, urbact_gnu)
allDB <- full_join(., eucicop_gnu)

## Correction coords of 20 cities (with same geonameId but not same coords)
# bibi <- allDB %>%
#   filter(duplicated(geonameId)) ## no need anymore

allDB <- allDB %>%  
  mutate_at(vars("members_etmun", "members_urbact",  
                 "participations_eucicop", "partners_eucicop", "projects_eucicop"), 
            replace_na, 0) 

allDB <- aggregate(x = allDB[3:9], 
                       by = list(geonameId = allDB$geonameId, asciiName = allDB$asciiName), 
                       FUN = max) 

rm (allGNU, bibi, etmun, urbact, eucicop)



# Add GN variables to allDB city
## Load DB GN full info

# OLD
# urbact_gnInfo <- readRDS("KEEP/AD/URBACT/UniqueGNforURBACT.rds")
# etmun_gnInfo <- readRDS("ETMUN/Data/UniqueGNforETMUN.rds")
# eucicop_gnInfo <- readRDS("KEEP/Data/GNid_uniqueCity_Eucicop.RDS")

# ## Correct Urbact
# urbact_gnInfo <- urbact_gnInfo %>%
#   rename(lat_GN = lat, lng_GN = lng ) %>%
#   mutate(lat_GN = as.numeric(lat_GN),
#          lng_GN = as.numeric(lng_GN))
#
# ## All in one
# GNinfoAll <- bind_rows(urbact_gnInfo, etmun_gnInfo) %>%
#   bind_rows(., eucicop_gnInfo) %>%
#   filter(!duplicated(geonameId))
#
# colnames(GNinfoAll)
# skim(GNinfoAll)
#
# ## clean columns
# GNinfoAll <- GNinfoAll %>%
#   select(geonameId, asciiName, countryCode, lng_GN, lat_GN,
#          continentCode, population, fcodeName, fcode,
#          everything()) %>% select(-distance, -score)
#
# ## save -- do not run again ever ever !!
# #saveRDS(GNinfoAll, "CITY/Data/UniqueGN_info_AllDB.rds")


# After correction
GNinfoAll <- readRDS("CITY/Data/UniqueGN_info_AllDB_Corr.rds")

## Finally add wanted info GN to DB city
allDB <- allDB %>% 
  left_join(., select(GNinfoAll, geonameId, fcodeName, fcode, 
                      countryCode, continentCode, population), 
            by = "geonameId")

## save
#saveRDS(allDB, "CITY/Data/DBCity.rds")

old <- readRDS("CITY/Data/DBCity_beforeCorr.RDS")
