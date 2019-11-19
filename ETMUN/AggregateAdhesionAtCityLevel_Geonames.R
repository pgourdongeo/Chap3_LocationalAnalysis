


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
