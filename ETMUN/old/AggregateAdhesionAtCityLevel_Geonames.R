


library(sf)
library(mapview)
library(tidyverse)
library(skimr)
library(geonames)  

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

### LOAD Result for the first query


sum(is.na(names(GNPlaceName)))

GNPlaceName[2706]
names(GNPlaceName)[2706] <- "Antananarivo"
GNPlaceNameDF <- bind_rows(GNPlaceName, .id = "Names") %>% complete(Names = names(GNPlaceName))

## Vérification names 

all.equal.character(EtmunPoints[1:7445,]$Locality_Siege, GNPlaceNameDF$Names)

EtmunPoints %>% slice(1:7445)%>% filter(is.na(Locality_Siege))
## join between original file and GN query
JoinEtmunGN1 <- EtmunPoints %>% left_join(GNPlaceNameDF, by= c("Locality_Siege"= "Names"), suffix = c("", ".GN"))

sum(is.na(TestJoin$toponymName))


## Get entities with no GN

NoGN <- JoinEtmunGN1 %>% filter(is.na(toponymName))


##2nd try


options(geonamesUsername="pgourdon")
GNplacename2 <- list()
NameLocality <- NoGN$Locality_Siege
for(i in NameLocality){
  
  GNplacename2[[i]] <- GNsearch(name = i, style = "FULL", maxRows = "1", cities = "cities1000")
  
}
saveRDS(GNplacename2, file = "GNplacename2.rds")

names(GNplacename2)[4847]
NameLocality2 <- NameLocality[4847:8397]


GNplacename3 <- list()

for(i in NameLocality2){
  
  GNplacename3[[i]] <- GNsearch(name = i, style = "FULL", maxRows = "1", cities = "cities1000")
  
}
saveRDS(GNplacename3, file = "GNplacename3.rds")


## Tranfo list result into df
which(is.na(names(GNplacename2)))
GNplacename2[387]
names(GNplacename2)[387] <- "Antananarivo"


GNPlaceNameDF2 <- bind_rows(GNplacename2, .id = "Names") %>% complete(Names = names(GNplacename2))
8397-4847

which(is.na(names(GNplacename3)))

GNPlaceNameDF3 <- bind_rows(GNplacename3, .id = "Names") %>% complete(Names = names(GNplacename3))

GNplacenameNoG <- bind_rows(GNPlaceNameDF2,GNPlaceNameDF3)
## Join

NoGN <- NoGN %>% select(1:20)
library(tidylog)
NoGNJoin <- NoGN %>% left_join(GNplacenameNoG, by= c("Locality_Siege"= "Names"), suffix = c("", ".GN"))
sum(is.na(NoGNJoin$geonameId))

## Intermediate File, result of the query on the names

GNfirstQuery <- JoinEtmunGN1 %>% filter(!is.na(toponymName))

GNnameETMUN <- bind_rows(GNfirstQuery, NoGNJoin)
 
length(unique(GNnameETMUN$MembershipCode))
GNnameETMUN <- GNnameETMUN %>% filter(!duplicated(MembershipCode))


### Control Geonames query, compute geographic and character distance

## Then make a query by long lat for entities with no or wrong geonames join 

saveRDS(GNnameETMUN, file = "GNEtmunBeforeCheck.rds")
list.files()
load("GNEtmunBeforeCheck.rds" )


##" Long lat dist 

library(geosphere)

GNEtmunGeocode <- GNEtmunBeforeCheck %>% filter_at(.vars = c(19,20,62,69), any_vars(!is.na(.)))  
GNEtmunGeocode$lng <- as.numeric(GNEtmunGeocode$lng)
GNEtmunGeocode$lat.GN <- as.numeric(GNEtmunGeocode$lat.GN)
GNEtmunGeocode<- GNEtmunGeocode %>% 
                      mutate(DistGeoGN =distHaversine(matrix(c(GNEtmunGeocode$lon, 
                                                               GNEtmunGeocode$lat), ncol = 2),
                                                              matrix(c(GNEtmunGeocode$lng, 
                                                                       GNEtmunGeocode$lat.GN), ncol = 2)))


  library(skimr)   
skim(GNEtmunGeocode)
GNEtmunGeocode %>% filter_at(.vars = c(19,20,62,69), any_vars(.< -360))                                                 

GNEtmunGeocode <- GNEtmunGeocode %>% select(MembershipCode, DistGeoGN)

GNEtmunCheckDistGeo <- GNEtmunBeforeCheck %>% left_join(GNEtmunGeocode)

## Filter ETMUN entities with distance superior with 2km with geonames query or with no GN results
GNEtmunPB <- GNEtmunCheckDistGeo %>% filter(DistGeoGN > 2000 | is.na(geonameId))
GNEtmunPB <- GNEtmunPB %>% filter(!is.na(lon))

######## New geonames query but by coord

library(geonames)  

options(geonamesUsername="pgourdon")

#1rst query
GNplacenameCoord <- list()
x <- GNEtmunPB$lon
y <- GNEtmunPB$lat
skim(y)
skim(x)
GNEtmunPB[GNEtmunPB$MembershipCode== "A11",  ]$lon
Id <- GNEtmunPB$MembershipCode
for(i in Id){
    x <- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lon
    y<- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lat
    GNplacenameCoord[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                   style = "FULL")
  }

saveRDS(GNplacenameCoord, file = "GNplacenameNearbyforPbandNoGN.rds")

# 2nd query
tail(GNplacenameCoord)
Id[1603]
Id[1604]

Id2 <- Id[1604:4260]
GNplacenameCoord2 <- list()

for(i in Id2){
  x <- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lon
  y<- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lat
  GNplacenameCoord2[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                 style = "FULL")
}

saveRDS(GNplacenameCoord2, file = "GNplacenameNearbyforPbandNoGN2.rds")

#3rd query

tail(GNplacenameCoord2)
Id2[1629]
Id2[1630]
which(Id2 == "A11027")

Id3 <- Id2[1630:2657]
GNplacenameCoord3 <- list()

for(i in Id3){
  x <- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lon
  y<- GNEtmunPB[GNEtmunPB$MembershipCode == i,]$lat
  GNplacenameCoord3[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                  style = "FULL")
}

saveRDS(GNplacenameCoord3, file = "GNplacenameNearbyforPbandNoGN3.rds")

GNEtmunPB_AfterNearBy <- bind_rows(GNplacenameCoord, GNplacenameCoord2 ,.id = "MembershipCode")
GNEtmunPB_df3 <- bind_rows(GNplacenameCoord3, .id = "MembershipCode")
GNEtmunPB_AfterNearBy <- bind_rows(GNEtmunPB_AfterNearBy , GNEtmunPB_df3)
