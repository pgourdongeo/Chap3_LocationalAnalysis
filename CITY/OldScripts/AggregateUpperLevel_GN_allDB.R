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
setwd("~/BD_Keep_Interreg/")

setwd("~/git/Chap3_LocationalAnalysis")

# library
library(tidylog)
library(tidyverse)
library(sf)



# Load DB
urbact <- readRDS("KEEP/AD/URBACT/URBACT_Membership_GNid.rds")
etmun <- readRDS("ETMUN/Data/ETMUN_Membership_GNid.rds")
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


# Load DB GN full info
urbact_gnInfo <- readRDS("KEEP/AD/URBACT/UniqueGNforURBACT.rds")
etmun_gnInfo <- readRDS("ETMUN/Data/UniqueGNforETMUN.rds")
eucicop_gnInfo <- readRDS("KEEP/Data/GNid_uniqueCity_Eucicop.RDS")

# Correct Urbact

urbact_gnInfo <- urbact_gnInfo %>% rename(lat_GN = lat, lng_GN = lng )%>% mutate(lat_GN = as.numeric(lat_GN),
                                                                                 lng_GN = as.numeric(lng_GN))
## All in one

GNinfoAll <- bind_rows(urbact_gnInfo, etmun_gnInfo) %>% bind_rows(., eucicop_gnInfo) 

GNinfoAll <- GNinfoAll %>% filter(!duplicated(geonameId))

colnames(GNinfoAll)

skim(GNinfoAll)


## Check doublon admin2 level


DuplicatedAdmin3 <- GNinfoAll %>% group_by(adminId3)%>% summarise(n= n())%>% filter(n>1)
rm(DuplicatedAdmin2)

## Chaeck problem
EuropeGN <- GNinfoAll %>% filter(continentCode == "EU")

EuropeGNsf <-st_as_sf( EuropeGN, coords = c("lng_GN", "lat_GN"), crs = 4326)%>% st_transform(crs=3035)

library(mapview)

mapview(EuropeGNsf)

## Check request hierarchy function
library(geonames)
options(geonamesUsername="pgourdon")

 dftest<- GNhierarchy(12047194) %>% slice(nrow(.)-1) %>% as.vector()
nrow( dftest)-1


####### Workflow filter non-conform entities (fdcodeName) and get the upper level hierarchy

PbEntities <- c("historical capital of a political entity",
                "religious populated place","section of populated place",
                "seat of a fifth-order administrative division", "historical populated place" )
GNpbinfra <- GNinfoAll %>% filter(fcodeName %in% PbEntities)


id <- GNpbinfra$geonameId

GNupperCity <- list()

for(i in id){
  
  GNupperCity[[i]] <- GNhierarchy(i) %>% slice(nrow(.)-1) 
}

upperHierachyGN <-bind_rows(GNupperCity, .id = "GNid")%>% complete(GNid = names(GNupperCity))
# 4 queries missing (Tant pis !!!!!)
## Check if new query exist in our database

GNdiff <- anti_join(upperHierachyGN,GNinfoAll, by=c("GNid" = "geonameId"))
# Yes

DicoUpperHierar <- upperHierachyGN %>% select(geonameId= GNid, NewGeonameId =geonameId) 

DicoUpperHierar <- DicoUpperHierar %>% mutate(NewGeonameId = ifelse(is.na(NewGeonameId), 
                                                                    geonameId,NewGeonameId))

GNinfoAll <- GNinfoAll %>% left_join(DicoUpperHierar)
skim(GNinfoAll)


GNinfoCleanAll <- GNinfoAll %>% filter(is.na(NewGeonameId)) 
GNinfoCleanAll <- GNinfoCleanAll %>% select(geonameId, asciiName, countryCode, lng_GN, lat_GN,
                                            continentCode, population, fcodeName, fcode, 
                                            everything()) %>% select(-NewGeonameId, -distance, -score)

saveRDS(GNinfoCleanAll, "UniqueGN_info_AllDB.rds")
saveRDS(DicoUpperHierar, "DicoGN_UpperHierarchy.rds")

pbGN <- GNhierarchy(731950)
pb2 <-  GNhierarchy(8030383)
pb2 <-  GNhierarchy(2595741)
pbParis <-GNhierarchy(6455259)
pbstras <-GNhierarchy(11071622)
GNhierarchy(560391)
############################## PB GN info all antijoin did not work 
################################and some of new upper level geonames are missing in GNinfo_allDB
#Other metho with LAU2 DB

LAU2sf <- st_read("LAU2/LAU2_6111_2015.geojson")

LAU2sf <- LAU2sf %>% st_make_valid() %>%st_cast( to= "POLYGON")


JoinPOintInLAU <- st_join(LAU2sf, EuropeGNsf, join = st_intersects)
# nb <- st_intersects(LAU2sf,EuropeGNsf)
# 
# count <- st_sf(n=sapply(X = nb,FUN = length), LAU2sf)

JoinPOintInLAU <- JoinPOintInLAU %>% filter(!is.na(geonameId))

Morethan1 <- JoinPOintInLAU %>% group_by(NSI_CODE11)%>% mutate(nGN= n())%>% filter(nGN > 1)

library(mapview)

mapview(Morethan1)+mapview(EuropeGNsfInLAU)

EuropeGNsfInLAU <- EuropeGNsf %>% filter(geonameId %in% Morethan1$geonameId)
