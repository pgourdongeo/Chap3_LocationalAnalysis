###############################################################################
#                                 BD city
#                          
# DESCRIPTION : création d'une base de données 'ville' à partir des trois bases
#               ETMUN, KEEP-EUCICOP et URBACT
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


# Load DB
urbact <- readRDS("KEEP/AD/URBACT/URBACT_Membership_GNidCorr.RDS")
etmun <- readRDS("ETMUN/Data/ETMUN_Membership_GNidCorr.RDS")
eucicop <- readRDS("KEEP/Data/sfParticipations_snapCorr.RDS")



# eucicop - sf to df
eucicop <- eucicop %>% 
  st_transform(crs = 4326)
  
coords <- as.data.frame(st_coordinates(eucicop))

eucicop <- eucicop %>% 
  mutate(lat_GN = coords$Y, lng_GN = coords$X) %>% 
  as.data.frame() %>% 
  select(-geometry) 

rm(coords)



# prepare data

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
  select(geonameId, members_etmun) %>% 
  filter(!duplicated(geonameId))


## urbact
urbact_gnu <- urbact %>% 
  select(geonameId, members_urbact) %>% 
  filter(!duplicated(geonameId)) 

## eucicop
eucicop_gnu <- eucicop %>% 
  select(geonameId, participations_eucicop, partners_eucicop, projects_eucicop) %>% 
  filter(!duplicated(geonameId)) 

## Compile
##
vec <- c(etmun_gnu$geonameId, urbact_gnu$geonameId, eucicop_gnu$geonameId)
allGNU <- data.frame(geonameId = unique(vec))

join1 <- full_join(etmun_gnu, urbact_gnu)
allDB <- full_join(join1, eucicop_gnu)




# Add GN variables to allDB city

# Load DB with gn info
gnInfo <- readRDS("UniqueGN_info_AllDB.rds")


#
allDB_gnInfo <- left_join(allDB, 
                          select(gnInfo, geonameId, asciiName, lng_GN, lat_GN, fcodeName))

table(allDB_gnInfo$fcodeName)
library(skimr)  
skim(allDB_gnInfo$geonameId)

eucicop <- readRDS("KEEP/Data/sfParticipations_snapCorr.RDS")
mapview(eucicop)
