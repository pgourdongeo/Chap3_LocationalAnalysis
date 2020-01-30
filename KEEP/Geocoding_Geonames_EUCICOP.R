###############################################################################
#                                DATA PREPARATION KEEP / EUCICOP
#                                Geocoding and geonames
#
# DESCRIPTION :  work on partners file to get geographic coordinates
# And Geonames information in order to agregate partners to city level (admin definition) 
# 
# PG
# November 2019
##############################################################################


### Packages
library(tidyverse)
library(skimr)
library(tidylog)
library(sf)
library(ggmap)
library(geonames)
library(countrycode)

####Load data
setwd("~/BD_Keep_Interreg/KEEP")

list.files("Data")


Partnership <- read.csv2("Data/ParntersEucicop_All.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(Partnership)

write.csv2(Partnership, "Data/Participations_All_Eucicop.csv", row.names = F, fileEncoding = "UTF-8")
saveRDS(Partnership,"Data/Participations_All_Eucicop.RDS" )
## Create a unique partner dataframe


Partners <- Partnership %>% filter(!duplicated(ID_PARTNER))

# Drop project info

Partners <- Partners %>% select(-Programme, -Acronym,-Lead.Partner, -ID_PARTICIPATION, -ID_PROJECT)


# Clean Town vector


Partners$Town <- str_to_title(Partners$Town)
Partners$Country <- str_to_title(Partners$Country)
Partners$CityAddress <- paste(Partners$Town, Partners$Country, sep = ", ")

Partners <- Partners %>% 
            mutate(CountryISO = countrycode(Partners$Country,"country.name", "iso2c"), 
                   Region = countrycode(Partners$Country,"country.name", "region"),
                   Continent = countrycode(Partners$Country,"country.name", "continent"))
skim(Partners)

unique(Partners$Country)
## Make a city df

City <- Partners %>% select(Town, Country, CountryISO,  Continent, CityAddress)

CityNoDuplicated <- City %>% filter(!duplicated(CityAddress))


########################### Geonames Query Name country
options(geonamesUsername="pgourdon")

#1st query
GNplacename<- list()


Id <- CityNoDuplicated$CityAddress
for(i in Id){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Town
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryISO
  GNplacename[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename, file = "GNplacename1.rds")
# CountryI
# [1] "FR"
# > i
# [1] "Daumazan, France"


GN1st <- bind_rows(GNplacename, .id = "CityAddress")
skim(GN1st)

#2nd query
GNplacename2 <- list()

IdGN1st <- GN1st$CityAddress
class(IdGN1st)
class(Id)
IdDf <- as.data.frame(Id)
Id2 <- IdDf %>% filter(!Id %in% IdGN1st)
Id2 <- Id2 %>% deframe()

for(i in Id2){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Town
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryISO
  GNplacename2[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename2, file = "GNplacename2.rds")
# CountryI
# [1] "FR"
# > i
# [1] "Daumazan, France"

## 3rd query
GNplacename2 <- readRDS("~/BD_Keep_Interreg/KEEP/GNplacename2.rds")
GN2nd <- bind_rows(GNplacename2, .id = "CityAddress")
skim(GN1st)

IdGN2nd <- GN2nd$CityAddress
IDGN1rstAnd2nd <- append(IdGN1st,IdGN2nd)

Id3 <- IdDf %>% filter(!Id %in% IDGN1rstAnd2nd)
Id3 <- Id3 %>% deframe()%>% as.character()


GNplacename3 <- list()

for(i in Id3){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Town
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryISO
  GNplacename3[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename3, file = "GNplacename3.rds")

## 4 th query

GNplacename3 <- readRDS("~/BD_Keep_Interreg/KEEP/GNplacename3.rds")
GN3rd <- bind_rows(GNplacename3, .id = "CityAddress")%>% complete(CityAddress = names(GNplacename3))


IdGN3rd <- GN3rd$CityAddress
IDGN1rstAnd2ndAnd3rd <- append(IDGN1rstAnd2nd,IdGN3rd)

Id4 <- IdDf %>% filter(!Id %in% IDGN1rstAnd2ndAnd3rd)
Id4 <- Id4 %>% deframe()%>% as.character()


GNplacename4 <- list()

for(i in Id4){
  nameI <- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$Town
  CountryI<- CityNoDuplicated[CityNoDuplicated$CityAddress == i,]$CountryISO
  GNplacename4[[i]] <-GNsearch(name = nameI , country = CountryI,  style = "FULL", maxRows = "1", cities = "cities1000")
}

saveRDS(GNplacename4, file = "GNplacename4.rds")

### All query in one df

GNplacename4 <- readRDS("~/BD_Keep_Interreg/KEEP/GNplacename4.rds")
GN4st <- bind_rows(GNplacename4, .id = "CityAddress")%>% complete(CityAddress = names(GNplacename4))


GNQueryNameCntrAll <- bind_rows(GN1st, GN2nd)
GNQueryNameCntrAll <- bind_rows(GNQueryNameCntrAll, GN3rd)
GNQueryNameCntrAll <- bind_rows(GNQueryNameCntrAll,GN4st)

saveRDS(GNQueryNameCntrAll, file = "GNQueryNameCntrAll_EucicopCities.rds")
skim(GNQueryNameCntrAll)


################# GEOCODING WITH GGMAP

################################"" Geocode 

## Maybe rename coord of GN lon.GN and lat.GN 
## to make sure no problem with the ggmap geocode results
GNQueryNameCntrAll <- GNQueryNameCntrAll %>% 
  rename(lng_GN = lng,
         lat_GN = lat)

library(ggmap)

register_google(key = "")

ggcoordEucicop <- mutate_geocode(GNQueryNameCntrAll, location = CityAddress)

saveRDS(ggcoordEucicop, "GNQueryNameCntrAll_EucicopCities_ggmap.rds" )
skim(ggcoordEucicop)




########### Compute distance to check geocoding processes (DO NOT RUN)
library(geosphere)

# Make sure coord are numeric
ggcoordEucicop$lng_GN <- as.numeric(ggcoordEucicop$lng_GN)
ggcoordEucicop$lat_GN <- as.numeric(ggcoordEucicop$lat_GN)

# Remove entities with no double coord
ggcoordEucicop_dblCoord <- ggcoordEucicop %>% 
  filter_at(.vars = c(22,29,53,54), all_vars(!is.na(.)))  

## Create a distance variable with geosphere 
ggcoordEucicop_dblCoord <- ggcoordEucicop_dblCoord %>% 
  mutate(DistGeoGN =distHaversine(matrix(c(ggcoordEucicop_dblCoord$lng_GN, 
                                           ggcoordEucicop_dblCoord$lat_GN), ncol = 2),
                                  matrix(c(ggcoordEucicop_dblCoord$lon, 
                                           ggcoordEucicop_dblCoord$lat), ncol = 2)))

skim(ggcoordEucicop_dblCoord$DistGeoGN)

ggcoordEucicop_dblCoord %>% filter(DistGeoGN > 10000)

saveRDS(ggcoordEucicop_dblCoord, "ggcoordEucicop_dblCoord.RDS")

### get geonames missing
ggcoordEucicop_noGN <- ggcoordEucicop %>% 
  filter(is.na(geonameId))
### rm ggcoord missing
ggcoordEucicop_noGN <- ggcoordEucicop_noGN %>% 
  filter_at(.vars = c(53,54), all_vars(!is.na(.))) 
saveRDS(ggcoordEucicop_noGN, "ggcoordEucicop_noGN.RDS")

### Geonames query by coord 
GNplacenameCoord <- list()
# define ID (address)
Id_noGN <- ggcoordEucicop_noGN$CityAddress
for(i in Id_noGN){
  # get  the lon
  x <- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lon
  #get the lat
  y<- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lat
  GNplacenameCoord[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                 style = "FULL")
}

GNCoord1st <- bind_rows(GNplacenameCoord, .id = "CityAddress")%>% complete(CityAddress = names(GNplacenameCoord))
skim(GNCoord1st)
saveRDS(GNCoord1st, "GNCoord1st.RDS")


##  2nd query on coord

GNplacenameCoord2 <- list()
# define ID (address)
tail(GNCoord1st)

Id_noGN[1599]
Id_noGN2 <-Id_noGN[1599:4627]
for(i in Id_noGN2){
  # get  the lon
  x <- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lon
  #get the lat
  y<- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lat
  GNplacenameCoord2[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                 style = "FULL")
}

saveRDS(GNplacenameCoord2, "GNplacenameCoord2.RDS")
GNCoord2nd <- bind_rows(GNplacenameCoord2, .id = "CityAddress")%>% complete(CityAddress = names(GNplacenameCoord2))
skim(GNCoord2nd)


## 3rd query 
GNplacenameCoord3 <- list()
tail(GNCoord2nd)

Id_noGN2[1582]
Id_noGN3 <-Id_noGN2[1582:3029]
for(i in Id_noGN3){
  # get  the lon
  x <- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lon
  #get the lat
  y<- ggcoordEucicop_noGN[ggcoordEucicop_noGN$CityAddress == i,]$lat
  GNplacenameCoord3[[i]] <- GNfindNearbyPlaceName(y, x, radius = "3", maxRows = "1",
                                                  style = "FULL")
}

saveRDS(GNplacenameCoord3, "GNplacenameCoord3.RDS")
GNCoord3rd <- bind_rows(GNplacenameCoord3, .id = "CityAddress")%>% complete(CityAddress = names(GNplacenameCoord3))
skim(GNCoord2nd)


############## Compute the dataframe of GN query on coords


GNcoordAll <- bind_rows(GNCoord1st, GNCoord2nd)
GNcoordAll <- bind_rows(GNcoordAll, GNCoord3rd)

skim(GNcoordAll)

ggcoordGoodat1st <- ggcoordEucicop %>% filter(!is.na(geonameId))

Noggmap<- ggcoordEucicop %>% 
  filter(is.na(geonameId))
### rm ggcoord missing
Noggmap <- Noggmap %>% 
  filter_at(.vars = c(53,54), all_vars(is.na(.))) 


### Join the results of the query coord with the original df ggcoordeucicop_noGN to get the long lat from ggmap

GNcoordAll <- GNcoordAll %>% rename(lat_GN = lat, lng_GN = lng)
GNcoordAllJoin <- GNcoordAll %>% left_join(select(ggcoordEucicop_noGN, CityAddress, lon, lat), by = "CityAddress")
GNcoordAllJoin$lat_GN <- as.numeric(GNcoordAllJoin$lat_GN)
GNcoordAllJoin$lng_GN <- as.numeric(GNcoordAllJoin$lng_GN)
GNcity <- bind_rows(ggcoordGoodat1st, GNcoordAllJoin)
GNcity <- bind_rows(GNcity, Noggmap)

skim(GNcity)

## 158 City Address with no geonames objects

#############" File to export

## The data frame of city address

write.csv2(GNcity, "Data/GNCityAddress_All.csv", row.names = F, fileEncoding = "UTF-8")
saveRDS(GNcity, "Data/GNCityAddress_All.RDS")

## unique Partner with key to join with geonames

GNcityForPartner <- GNcity %>% select(CityAddress,geonameId,asciiName, lng_GN, lat_GN)

PartnersGNid <- Partners %>% left_join(GNcityForPartner, by = "CityAddress")
str(PartnersGNid)
write.csv2(PartnersGNid, "Data/UniquePartners_GNid_Eucicop.csv",row.names = F, fileEncoding = "UTF-8")

max(PartnersGNid$lng_GN, na.rm = T)
min(PartnersGNid$lng_GN, na.rm = T)
max(PartnersGNid$lat_GN, na.rm = T)
min(PartnersGNid$lat_GN, na.rm = T)
saveRDS(PartnersGNid, "Data/UniquePartners_GNid_Eucicop.RDS")

# The 158 cityAddress with no geonames represent only 219 partners with no geocode and geonames ID

### Unique city 

GNidUniqueCity <- GNcity %>% select(-CityAddress)%>%filter(!duplicated(geonameId))

skim(GNidUniqueCity)

write.csv2(GNidUniqueCity,"Data/GNid_uniqueCity_Eucicop.csv", row.names = F, fileEncoding = "UTF-8" )
saveRDS(GNidUniqueCity, "Data/GNid_uniqueCity_Eucicop.RDS")

## Check big errors country

ErrorsCity <- CityNoDuplicated %>% left_join(GNcity, by = "CityAddress")
skim(ErrorsCity)
ErrorsCityCountry <- ErrorsCity %>% filter(CountryISO != countryCode)
