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
urbact <- readRDS("KEEP/AD/URBACT/URBACT_Membership_GNid.RDS")
etmun <- readRDS("ETMUN/Data/ETMUN_Membership_GNid.RDS")
eucicop <- readRDS("KEEP/Data/sfParticipations_snap.RDS")



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
  select(geonameId, asciiName, lat_GN, lng_GN, participations_eucicop, partners_eucicop, projects_eucicop) %>% 
  filter(!duplicated(geonameId)) 

## Compile
##
vec <- c(etmun_gnu$geonameId, urbact_gnu$geonameId, eucicop_gnu$geonameId)
allGNU <- data.frame(geonameId = unique(vec))

join1 <- full_join(etmun_gnu, urbact_gnu)
allDB <- full_join(join1, eucicop_gnu)

bibi <- allDB %>% 
  filter(duplicated(geonameId))

allDB <- allDB %>% 
  mutate_at(vars("members_etmun", "members_urbact", 
                 "participations_eucicop", "partners_eucicop", "projects_eucicop"),
            replace_na, 0)

allDB_gnu <- aggregate(x = allDB[3:9],
                     by = list(geonameId = allDB$geonameId, asciiName = allDB$asciiName),
                     FUN = max)



# Add GN variables to allDB_GNU

# Load DB with gn info
urbact_gnInfo <- readRDS("KEEP/AD/URBACT/UniqueGNforURBACT.rds")
etmun_gnInfo <- readRDS("ETMUN/Data/UniqueGNforETMUN.rds")
eucicop_gnInfo <- readRDS("KEEP/Data/GNid_uniqueCity_Eucicop.RDS")


urbact_gnInfo <- urbact_gnInfo %>% 
  rename(lat_GN = lat,
         lng_GN = lng) %>% 
  mutate(lat_GN = as.numeric(lat_GN),
         lng_GN = as.numeric(lng_GN))


allGN_info <- eucicop_gnInfo %>% 
  bind_rows(etmun_gnInfo) %>% 
  bind_rows(urbact_gnInfo) %>% 
  filter(!duplicated(geonameId))


EuropeGN <- allGN_info %>% 
  filter(continentCode == "EU")
EuropeGNsf <-st_as_sf( EuropeGN, coords = c("lng_GN", "lat_GN"), crs = 4326)

mapview(EuropeGNsf)

admin <- allGN_info %>% 
  filter(continentCode == "EU") %>% 
  group_by(adminId3) %>% 
  summarise(n = n()) %>% 
  filter(n>1)

unique(allGN_info$fcodeName)

typo <- allGN_info %>% 
  group_by(fcodeName) %>% 
  summarise(n = n())


library(geonames)
options(geonamesUsername="")

dftest<- GNhierarchy(12047194) %>% slice(nrow(.)-1)


  

