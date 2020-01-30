


library(sf)
library(mapview)
library(tidyverse)
library(skimr)
library(geonames)
library(tidylog)

## Load SF
setwd("~/BD_Keep_Interreg/ETMUN")

EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F)
skim(EtmunPoints)
## Clean DF text with str_to_title


EtmunPoints$Locality_Siege<- str_to_title(EtmunPoints$Locality_Siege)
EtmunPoints$CountryName<- str_to_title(EtmunPoints$CountryName)
#Address
EtmunPoints$CityAddress <- paste(EtmunPoints$Locality_Siege, EtmunPoints$CountryName, sep = ", ")


## Make a city df no duplicated
City <- EtmunPoints %>% select(Locality_Siege, CountryName, CountryCode,  Continent, CityAddress)

CityNoDuplicated <- City %>% filter(!duplicated(CityAddress))



## GEONAMES



options(geonamesUsername="pgourdon")

# GNTest <-GNsearch(name = sampleEtmun$Locality_Siege, style = "FULL", maxRows = "1", cities = "cities1000")
# FR<-GNcountryInfo("FR")
# gntest<-GNchildren(3017382, hierarchy = "tourism")
# GNhierarchy(3017382)
# GNsiblings(3017382)
# source(system.file("tests","testing.R",package="geonames"),echo=TRUE)

# 1st QUERY
GNplacename <- list()

Id <- CityNoDuplicated$CityAddress
for(i in Id){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Locality_Siege
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryCode
  GNplacename[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename, file = "GNplacename.rds")

GN1st <- bind_rows(GNplacename, .id = "CityAddress") %>% complete(CityAddress = names(GNplacename))
skim(GN1st)
sum(is.na(names(GNplacename)))

#2nd QUERY


Id[length(GNplacename)]
Id[length(GNplacename)+1]
Stop<- length(GNplacename)+1
#2nd query
GNplacename2 <- list()

Id2 <- Id[Stop: length(Id)]

for(i in Id2){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Locality_Siege
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryCode
  GNplacename2[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename2, file = "GNplacename2.rds")

GNplacename2 <- readRDS("GNplacename2.rds")

GN2nd <- bind_rows(GNplacename2, .id = "CityAddress") %>% complete(CityAddress = names(GNplacename2))
#  3rd Query 

Id2[length(GNplacename2)]
Id2[length(GNplacename2)+1]

Stop<- length(GNplacename2)+1


GNplacename3 <- list()

Id3 <- Id2[Stop: length(Id2)]

for(i in Id3){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Locality_Siege
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryCode
  GNplacename3[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename3, file = "GNplacename3.rds")

GN3rd <- bind_rows(GNplacename3, .id = "CityAddress") %>% complete(CityAddress = names(GNplacename3))




## Merge a single dataframe with all queries

GNQueryNameCntrAll <- bind_rows(GN1st, GN2nd)
GNQueryNameCntrAll <- bind_rows(GNQueryNameCntrAll, GN3rd)

saveRDS(GNQueryNameCntrAll, file = "GNplacenameETMUN.rds")

skim(GNQueryNameCntrAll)

GNQueryNameCntrAll <- readRDS(file = "GNplacenameETMUN.rds")

############### GET citis with no GN


NoGN <- GNQueryNameCntrAll %>% filter(is.na(geonameId))

NoGN <- NoGN %>% select(CityAddress)

NoGNggmap <- NoGN %>% left_join(select(EtmunPoints, CityAddress, lon, lat))%>% distinct()
skim(NoGNggmap)

### QUERY By Lon Lat from ggmap
# Filter the 74 entities with no ggmap lon lat
NoGNggmap <- NoGNggmap %>% filter(!is.na(lon))


### 1st query

options(geonamesUsername="pgourdon")
GNplacenameCoord <- list()
Id <- NoGNggmap$CityAddress

for(i in Id){
  x <- NoGNggmap[NoGNggmap$CityAddress == i,]$lon
  y<- NoGNggmap[NoGNggmap$CityAddress == i,]$lat
  GNplacenameCoord[[i]] <- GNfindNearbyPlaceName(y, x, radius = "5", maxRows = "1",
                                                 style = "FULL")
}



saveRDS(GNplacenameCoord, file = "GNplacenameCoord.rds")

GNbyCoord1st <- bind_rows(GNplacenameCoord, .id = "CityAddress") %>% complete(CityAddress = names(GNplacenameCoord))

skim(GNbyCoord1st)


### Second query on coord

Id[length(GNplacenameCoord)]
Id[length(GNplacenameCoord)+1]

Stop<- length(GNplacenameCoord)+1


GNplacenameCoord2 <- list()

Id2 <- Id[Stop: length(Id)]

for(i in Id2){
  x <- NoGNggmap[NoGNggmap$CityAddress == i,]$lon
  y<- NoGNggmap[NoGNggmap$CityAddress == i,]$lat
  GNplacenameCoord2[[i]] <- GNfindNearbyPlaceName(y, x, radius = "5", maxRows = "1",
                                                 style = "FULL")
}



saveRDS(GNplacenameCoord2, file = "GNplacenameCoord2.rds")

GNbyCoord2nd <- bind_rows(GNplacenameCoord2, .id = "CityAddress") %>% complete(CityAddress = names(GNplacenameCoord2))


### Combine
GNQueryCoord <- bind_rows(GNbyCoord1st, GNbyCoord2nd)

saveRDS(GNQueryCoord, file = "GNplacenameCoordAll.rds")

skim(GNQueryCoord)

GNQueryCoord <- readRDS(file = "GNplacenameCoordAll.rds")

########### Combine all city info GN + ggmap in one dataframe

## df of city with ggmap lon lat

CityNoDuplicatedGN  <- CityNoDuplicated %>% left_join(select(EtmunPoints, CityAddress, lon, lat))%>% distinct()

# Replace Query coord into the GN on name

GNall <- GNQueryNameCntrAll %>%
  filter(is.na(geonameId)) %>% 
  select(CityAddress) %>%
 left_join(GNQueryCoord, by = "CityAddress") %>% 
  bind_rows(., anti_join(GNQueryNameCntrAll,., by = "CityAddress"))

skim(GNall)

GNall <- GNall %>% rename(GN_lon = lng, GN_lat = lat)%>% mutate(GN_lon = as.numeric(GN_lon),GN_lat = as.numeric(GN_lat) )
# GNall <- GNall  %>% mutate(GN_lon = as.numeric(GN_lon),GN_lat = as.numeric(GN_lat) )

CityNoDupggmapGN <- CityNoDuplicatedGN %>% left_join(GNall, by = "CityAddress" )

skim(CityNoDupggmapGN)


#### Explore the problem of geo codage

ggmapPB <- CityNoDupggmapGN %>% filter(is.na(lon))

GNpb <- CityNoDupggmapGN %>% filter(is.na(geonameId))


## Distance check
library(geosphere)

 

DistanceCheck <- CityNoDupggmapGN %>% filter_at(.vars = c(6,7,22,28), any_vars(!is.na(.))) %>%
  mutate(DistGeoGN =distHaversine(matrix(c(.$lon, 
                                           .$lat), ncol = 2),
                                  matrix(c(.$GN_lon, 
                                           .$GN_lat), ncol = 2)))

PbDist <- DistanceCheck %>% filter(DistGeoGN > 5000 | is.na(DistGeoGN))


CoordPB <-  CityNoDupggmapGN %>% filter(is.na(lon)|is.na(geonameId))

PbAllManualCorrect <- bind_rows(PbDist,CoordPB)
write.csv2(PbAllManualCorrect, "ManualCorrectionGNggmapETMUN.csv", row.names = F)


##" Remplacer les entités corrigées GN (inner join + antijoin ? ) + Puis virer ceux qui manquent. 
# Vérifier les duplicatas GeonamesId mais faire jointure pour avoir Ascii Name et Geonames ID sur EtmunPoints


#### Import manual corrections and last GeoNames Query

manualcorrect <- read.csv2("ETMUN/ManualCorrectionGNggmapETMUN2.csv", stringsAsFactors = F)

longlatLastQuery <- manualcorrect %>% filter(typeofcorrect == "lonlat")

GNlastcorrectLonLat <- list()
Id <- longlatLastQuery$CityAddress
for(i in Id){
  x <- longlatLastQuery[longlatLastQuery$CityAddress == i,]$lon
  y<- longlatLastQuery[longlatLastQuery$CityAddress == i,]$lat
  GNlastcorrectLonLat[[i]] <- GNfindNearbyPlaceName(y, x, radius = "10", maxRows = "1",
                                                  style = "FULL")
}



NamesLastQuery <-  manualcorrect %>% filter(typeofcorrect == "NameCtr")


Id <- NamesLastQuery$CityAddress

GNlastcorrectNames <- list()


for(i in Id){
  nameI <- NamesLastQuery[NamesLastQuery$CityAddress == i,]$Locality_Siege
  CountryI<- NamesLastQuery[NamesLastQuery$CityAddress == i,]$CountryCode
  GNlastcorrectNames[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1")
}


skim(CityNoDupggmapGN)




## Combine last query on manual correct

GNbyCoordManual <- bind_rows(GNlastcorrectLonLat  , .id = "CityAddress") %>% complete(CityAddress = names(GNlastcorrectLonLat ))

GNbyNameManual <- bind_rows(GNlastcorrectNames  , .id = "CityAddress") %>% complete(CityAddress = names(GNlastcorrectNames))

GNManualCOrrectAll <- bind_rows(GNbyCoordManual, GNbyNameManual)


ETMUNfinalGN <- GNall %>% 
  anti_join(.,GNManualCOrrectAll, by = "CityAddress")%>%
  bind_rows(., GNManualCOrrectAll)

ETMUNfinalGN <- ETMUNfinalGN %>% mutate(lat = as.numeric(lat), lng = as.numeric(lng))
ETMUNfinalGN <- ETMUNfinalGN %>% mutate(GN_lat = ifelse(is.na(GN_lat),lat, GN_lat), GN_lon = ifelse(is.na(GN_lon), lng, GN_lon))
skim(ETMUNfinalGN)

ETMUNfinalGN2 <- ETMUNfinalGN %>% filter(!duplicated(CityAddress))%>%
  filter(!is.na(geonameId))
ETMUNfinalGN2 <- ETMUNfinalGN2 %>% rename(lng_GN =GN_lon, lat_GN = GN_lat)


saveRDS(ETMUNfinalGN2, "Data/ETMUN_GNall_UniqueCity.rds")


ETMUNClean <- EtmunPoints %>% select(-Adress)

GNetmunKeyJoin <- ETMUNfinalGN2 %>% select(CityAddress, geonameId, asciiName, lng_GN, lat_GN)


ETMUN_KeyGN <- ETMUNClean %>% left_join(GNetmunKeyJoin)

skim(ETMUN_KeyGN)
## 9 lon lat (GN) missing and 13390 unique GN id
ETMUN_KeyGN <- ETMUN_KeyGN %>% select(-X,-Y)

write.csv2(ETMUN_KeyGN, file = "Data/ETMUN_Membership_GNid.csv", row.names = F, fileEncoding = "UTF-8")
saveRDS(ETMUN_KeyGN, file = "Data/ETMUN_Membership_GNid.rds")

### Create a DF of Unique GN for ETMUN

skim(ETMUNfinalGN2)

UniqueGNforETMUN <- 
  ETMUNfinalGN2 %>%select(-lat, -lng, - CityAddress, - distance, -score)%>% distinct() %>% filter(!duplicated(geonameId))

write.csv2(UniqueGNforETMUN, file = "Data/UniqueGNforETMUN.csv", row.names = F, fileEncoding = "UTF-8")
saveRDS(UniqueGNforETMUN, file = "Data/UniqueGNforETMUN.rds")
