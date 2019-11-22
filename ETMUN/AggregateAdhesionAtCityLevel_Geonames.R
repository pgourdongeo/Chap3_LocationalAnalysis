


library(sf)
library(mapview)
library(tidyverse)


## Load SF
setwd("~/BD_Keep_Interreg/ETMUN")

EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F)


### Test Geonames


sampleEtmun <- EtmunPoints %>% sample_n(size = 50)


library(geonames)  

options(geonamesUsername="pgourdon")

# GNTest <-GNsearch(name = sampleEtmun$Locality_Siege, style = "FULL", maxRows = "1", cities = "cities1000")
GNcountryInfo("KE")
# source(system.file("tests","testing.R",package="geonames"),echo=TRUE)

GNplacename <- list()
NameLocality <- EtmunPoints$Locality_Siege
for(i in NameLocality){
  
  GNplacename[[i]] <- GNsearch(name = i, style = "FULL", maxRows = "1", cities = "cities1000")
  
}
saveRDS(GNplacename, file = "GNplacename.rds")
GNPlaceNamedTest <- GNfindNearbyPlaceName(lat = sampleEtmun$lat, lng = sampleEtmun$lon, radius = "5", maxRows = "1", style = "MEDIUM")


### LOAD Result for the first query

GNPlaceName <- readRDS("GNplacename.rds")
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
library(geonames)  

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
