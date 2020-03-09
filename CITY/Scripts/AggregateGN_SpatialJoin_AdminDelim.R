##==========================================================================##         
##            Aggrégation de la base ville (GN) avec une jointure spatiale  ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base City (compilation ETMUN/EUCICOP/ URBACT)              ##
##               au niveau des localités                                    ##
##                                                                          ##
## PG, Fev 2020                                                             ##
##==========================================================================##


# CONTENTS
# 1. Load Data
# 2. Spatial Joint   
# 3. Aggregate GN in same Admin polygon (keep the most populated Populated Place)

##Packages
library(tidyverse)
library(sf)
library(tidylog)
library(readr)
library(mapview)
library(skimr)

# Working directory huma-num
setwd("~/BD_Keep_Interreg/CITY/")


AdminEU <- readRDS("Data/AdminDelimPop0611.RDS")
AdminEU  <- AdminEU %>% rename( COMM_ID = Code_2, NAME_ASCI =  Name_2)

GNall <- readRDS("Data/DBCity.rds")

GNallsf <- st_as_sf(GNall, coords = c("lng_GN", "lat_GN") , crs = 4326) %>% st_transform(crs= 3035)

skim(GNall)
### Test on Paris to create Function =======

# Get cities conflict (several points in the same polygon)
# ParisSf <- AdminEU %>% filter(COMM_ID == "FR1175056")
# 
# GNParis <- GNallsf %>% filter(str_detect(asciiName, "Paris"))
# 
# 
# Inter <- st_within(GNParis, ParisSf, sparse = T) %>% as.data.frame()%>% rename(Poly.id = col.id, CityId = row.id)
# 
# 
# ParisConflict <-  GNParis %>% 
#                     mutate( CityId = row_number() ) %>%
#                  left_join(Inter)%>% group_by(Poly.id) %>% filter( n() > 1 )%>% ungroup()
# 
# 
# # Aggregate by pop
# 
# 
# 
# ParisConflict$population <- as.numeric(ParisConflict$population)
# 
# # This will apply the function in FUN to each subset of d defined by a unique poly.id
# polygons <- by(d=ParisConflict, INDICES = ParisConflict$Poly.id , FUN = function(d){
#   
#   # Identify all the rows with fcode that starts with P
#   startswithP <- d[substr(d$fcode,1,1)=="P",]
#   
#   # Now get the row with the highest population from those rows
#   # There might be more than one of these so just take the first row.
#   maxpop <- startswithP[startswithP$population == max(startswithP$population),][1,]
#   
#   # Now construct the row of my dataframe.
#   # First sum all the numeric rows of d.
#   # Then identify all the character rows of maxpop
#   data.frame(geonameId = maxpop$geonameId, 
#              asciiName = maxpop$asciiName, 
#              members_etmun = sum(d$members_etmun), 
#              members_urbact = sum(d$members_urbact),
#              participations_eucicop = sum(d$participations_eucicop),
#              partners_eucicop = sum(d$partners_eucicop),
#              projects_eucicop = sum(d$projects_eucicop),
#              fcodeName = maxpop$fcodeName,
#              fcode = maxpop$fcode,
#              countryCode = maxpop$countryCode,
#              continentCode = maxpop$continentCode,
#              population = maxpop$population,
#              CityId = maxpop$CityId,
#              Poly.id = maxpop$Poly.id,
#              geometry = maxpop$geometry) 
#              
# })
# colnames(ParisConflict)
# # Now I have a list of one-row dataframes I can rbind them together.
# Paris.perpolygon <- do.call(rbind, polygons) 
# class(Paris.perpolygon)



## Functions ======

# Get Cities conflict
GetMultiPointsPoly <- function(sfpoints,sfpoly){
  require(tidyverse)
  require(sf)
 
 
InterDf <- st_within(sfpoints, sfpoly, sparse = T) %>% as.data.frame()%>% rename(Poly.id = col.id, City.id = row.id)
  
  CitiesConflict <- sfpoints %>% 
                    mutate( City.id = row_number() ) %>%
                    left_join(InterDf)%>%
                     filter(!is.na(Poly.id))%>%
                    group_by(Poly.id) %>% 
                    filter( n() > 1 )%>% 
                    ungroup()
                    
  
  sfpoly <- sfpoly %>% 
            mutate(Poly.id = row_number())%>%
            st_drop_geometry()
            
  
  CitiesConflict <- CitiesConflict %>% 
                    left_join(sfpoly)
    
  print(paste0("Number of Polygons with several points : ", length(unique(CitiesConflict$Poly.id))))
  return(CitiesConflict)
}

GetUniquePointPoly <- function(sfpoints,sfpoly){
  require(tidyverse)
  require(sf)
  
  
  InterDf <- st_within(sfpoints, sfpoly, sparse = T) %>% as.data.frame()%>% rename(Poly.id = col.id, City.id = row.id)
  
  CitiesOkay <- sfpoints %>% 
    mutate( City.id = row_number() ) %>%
    left_join(InterDf)%>%
    filter(!is.na(Poly.id))%>%
    group_by(Poly.id) %>% 
    filter( n() == 1 )%>% 
    ungroup()
    
  
  sfpoly <- sfpoly %>% 
    mutate(Poly.id = row_number())%>%
    st_drop_geometry()
  
  
  CitiesOkay <-  CitiesOkay  %>% 
    left_join(sfpoly)
  
  return(CitiesOkay)
}

GetPointOutsidePoly <- function(sfpoints,sfpoly){
  require(tidyverse)
  require(sf)
  
  
  InterDf <- st_within(sfpoints, sfpoly, sparse = T) %>% as.data.frame()%>% rename(Poly.id = col.id, City.id = row.id)
  
  CitiesNA<- sfpoints %>% 
    mutate( City.id = row_number() ) %>%
    left_join(InterDf)%>%
    filter(is.na(Poly.id))
  
  return(CitiesNA)
}
# Aggregate

AggregatePopMax <- function(sfpoints){
res <- by(d=sfpoints, INDICES = list(sfpoints$Poly.id) , FUN = function(d){
    
    # Identify all the rows with fcode that starts with P
    startswithP <- d[substr(d$fcode,1,1)=="P",]
    
    # Now get the row with the highest population from those rows
    # There might be more than one of these so just take the first row.
    maxpop <- startswithP[startswithP$population == max(startswithP$population),][1,]
    
    # Now construct the row of my dataframe.
    # First sum all the numeric rows of d.
    # Then identify all the character rows of maxpop
    data.frame(geonameId = maxpop$geonameId, 
               asciiName = maxpop$asciiName, 
               members_etmun = sum(d$members_etmun), 
               members_urbact = sum(d$members_urbact),
               participations_eucicop = sum(d$participations_eucicop),
               partners_eucicop = sum(d$partners_eucicop),
               projects_eucicop = sum(d$projects_eucicop),
               fcodeName = maxpop$fcodeName,
               fcode = maxpop$fcode,
               countryCode = maxpop$countryCode,
               continentCode = maxpop$continentCode,
               population = maxpop$population,
               City.id = maxpop$City.id,
               Poly.id = maxpop$Poly.id,
               geometry = maxpop$geometry,
               COMM_ID = maxpop$COMM_ID,
               NAME_ASCI = maxpop$NAME_ASCI,
               PopAdmin06 = maxpop$PopAdmin06,
               PopAdmin11 = maxpop$PopAdmin11, stringsAsFactors = F) 
    
  })

  # Now I have a list of one-row dataframes I can rbind them together.
  resdf <- do.call(rbind, res) 
  resdf <- st_sf( resdf, coords =  resdf$geometry, crs = 3035)
  return(resdf)
}

#####  Perform aggregation on the dataset====
## get conflict cities

CitiesConflict <-  GetMultiPointsPoly(sfpoints = GNallsf, sfpoly = AdminEU)
CitiesConflict$population <- as.numeric(CitiesConflict$population)
# Look for duplicate GNid
skim(CitiesConflict)

##aggregate conflict cities

AggregateGNPoly <-AggregatePopMax(CitiesConflict)
AggregateGNPoly <- AggregateGNPoly %>% filter(!is.na(geonameId))
## Get Unique cities in polygon

UniqueCities <- GetUniquePointPoly(sfpoints = GNallsf, sfpoly = AdminEU)
skim(UniqueCities)
##Get outside Poly cities 

OutsidePoly <- GetPointOutsidePoly(sfpoints = GNallsf, sfpoly = AdminEU)

 (nrow(CitiesConflict)+nrow(UniqueCities)+nrow(OutsidePoly)) - nrow(GNall) 

skim(OutsidePoly)

skim(AggregateGNPoly)
#### Remind to deal with doublon (no doublons)


### Chack doublons GN ID
#Cities conflict
# skim(CitiesConflict)
# CitiesConflict %>% filter(duplicated(geonameId))
# Doublons <- CitiesConflict %>% group_by(geonameId)%>% summarise(n=n())%>% filter(n>1)
# Doublons <- CitiesConflict %>% filter(geonameId %in% Doublons$geonameId)
# mapview(Doublons) + 
#   mapview(AdminEU[AdminEU$COMM_ID %in% Doublons$COMM_ID,])
# 
# # Unique Cities
# 
# Doublons <- UniqueCities %>% group_by(geonameId)%>% summarise(n=n())%>% filter(n>1)
# Doublons <- UniqueCities%>% filter(geonameId %in% Doublons$geonameId)
# mapview(Doublons) + 
#   mapview(AdminEU[AdminEU$COMM_ID %in% Doublons$COMM_ID,])
# 
# 78/2
# 
# 39+4
### Create a correspondance table and outputs for correction ====

## Create a correspondance table between aggregated GN id and original GN id


DicoPolyNewGNid <- AggregateGNPoly %>% select(geonameId, Poly.id)%>% st_drop_geometry()
DicoAggrGN <- CitiesConflict %>%
              select(geonameId, Poly.id)%>%
              st_drop_geometry()%>% 
              rename(OldGNid = geonameId)%>% 
                left_join(DicoPolyNewGNid)

# Two NAs in the dico file, Aggregation of Trojir in Croatia fails.
# Manual correct
DicoAggrGN[DicoAggrGN$OldGNid == 	6620322, ]$geonameId <-	3188763
DicoAggrGN[DicoAggrGN$OldGNid == 	3188763, ]$geonameId <-	3188763

Trojir <- CitiesConflict[CitiesConflict$geonameId == 3188763 ,]
TrojirToAggregate <- CitiesConflict[CitiesConflict$geonameId == 6620322 ,]

Trojir[1,]$members_etmun <- Trojir[1,]$members_etmun +1
Trojir$coords <- Trojir[1,]$geometry
AggregateGNPoly <- rbind(AggregateGNPoly,Trojir)

# Erase same old GNid and New GNid (aggregated  entities)

DicoAggrGN <- DicoAggrGN %>% filter( geonameId != OldGNid)

DicoAggrGN <- DicoAggrGN %>% select(-Poly.id)
skim(DicoAggrGN)


saveRDS(DicoAggrGN, "Data/DicoAggrGN.rds")
# Unique GN info ALL

GNinfoAll <- readRDS("Data/UniqueGN_info_AllDB.rds")

OldGNtoRemove <- DicoAggrGN$OldGNid

GNinfoAll <- GNinfoAll %>% filter(!geonameId %in% OldGNtoRemove)#length 19243

nrow(AggregateGNPoly)+nrow(UniqueCities)+nrow(OutsidePoly)

GNinfoAll <- GNinfoAll %>% filter(!is.na(geonameId))

saveRDS(GNinfoAll, "Data/UniqueGN_info_AllDB_Corr.rds")
# mapview(CitiesConflict[CitiesConflict$geonameId == 2964892,])+ mapview(AdminEU[AdminEU$COMM_ID == "IE7",])
