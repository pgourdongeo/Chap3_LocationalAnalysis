


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


GNPlaceNameDF <- bind_rows(GNPlaceName, .id = "Names") %>% complete(Names = names(GNPlaceName))

GNPlaceName[1]$Seydikemer
names(GNPlaceNameDF)
GNPlaceNameDF2 <- setNames(data.frame(matrix(ncol = length(GNPlaceNameDF), nrow = 0)), names(GNPlaceNameDF))

GNPlaceName3 <-  lapply(GNPlaceName, function(x) if(is.null(x)) data.frame(C1 = NA) else x)

for(i in GNPlaceName){
  ifelse(length(i) == 0, GNPlaceNameDF2[i,] <- NA)
  

}

l <- list(Name1 = data.frame(), 
                 Name2 = data.frame(V1 = "A", V2 = "B", stringsAsFactors = F), 
                 Name3 = data.frame(V1="B", V2= NA, V3 ="C", stringsAsFactors = F))
l

library(dplyr)
df <- bind_rows(l,.id = "NAME") %>% complete(NAME = names(l))
df
data.frame(rbind(Name1,Name2,Name3))

bind_rows(l, .id = "Names") %>%
  complete(Names = names(l))
names(GNPlaceName)
 dput(head(GNPlaceName))
 
 samplelist <- sample(GNPlaceName, 1000)

 
 sample_n(GNPlaceName, 4000, replace = FALSE)
 
 dput( sample_n(GNPlaceName, 30))

 names(GNPlaceName)[2706] <- "bug bug bug"
 samplelist <- sample(GNPlaceName, 100)
   df <- bind_rows(GNPlaceName, .id = "Names") %>% complete(Names = names(GNPlaceName))
   df %>% head() %>% pull(asciiName)
length(unique(names(GNPlaceName)))
