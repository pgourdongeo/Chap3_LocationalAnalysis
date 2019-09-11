

library(tidyverse)
library(sf)
library(mapview)
PartnersGeoCode <- read.csv2("KEEP/AD/PartnersGeoCode.csv", stringsAsFactors = F)


## Transfor into sf object

SP_PartnersAll <- st_as_sf(PartnersGeoCode, coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3035)


SP_PartnersAll



mapview(SP_PartnersAll)
