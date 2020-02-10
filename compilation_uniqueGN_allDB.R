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
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis")

# library
library(tidylog)
library(tidyverse)
library(sf)



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


test <- allDB_gnu %>% 
  mutate(detect = str_detect(allDB_gnu$asciiName, " ")) %>% 
  filter(detect == TRUE)



