###############################################################################
# NAME
# DESCRIPTION
# AUTHOR
# DATE
##############################################################################


setwd("~/git/Chap3_LocationalAnalysis/KEEP")

library(readr)
library(tidyverse)
library(tidylog)
library(skimr)
#library(photon)
#library(countrycode)
library(cartography)
library(sf)
library(ggmap)
library(mapview)
library(purrr)


# Projects <- read.csv2("DataSource/Keep_ClosedProject_LeadPartner_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")


# Import and format data
Partners <- read_delim("DataSource/Keep_ClosedProject_Partner.csv", 
                       ";", escape_double = FALSE, trim_ws = TRUE)
Partners <- Partners %>% 
  transmute(Programme, Acronym, Project.Partner = `Project Partner`,
            Lead.Partner = `Lead Partner`, Street, Postal.code =`Postal code`,
            Town, Country, Website, Department, Legal.Status = `Legal Status`,
            Country.Code = `Country code`, NUTS1 = `NUTS1 (or equivalent)`,
            NUTS2 = `NUTS2 (or equivalent)`, NUTS3 = `NUTS3 (or equivalent)`)

skim(Partners)

Partners$Street[is.na(Partners$Street)] <- ""  
Partners$Location <- paste(Partners$Street, ", ", 
                           Partners$Postal.code," ",
                           Partners$Town,", ",
                           Partners$Country, sep = "")
Partners$Location <- tolower(Partners$Location)

PartnerLocation <- Partners$Location %>% unique()


# Geocoding participation places with gmap

register_google(key = "")

ggcoord <- ggmap::geocode(PartnerLocation, output = "more")

## save
ggcoord$Location <- PartnerLocation
write.csv2(ggcoord, "DataSource/ggcoordPartners.csv")

## join coord to the db
Partners <- left_join(x = Partners, y = select(ggcoord, Location, lon, lat), by = "Location")


## Add coord manual corrected
noCoord <- Partners %>% 
  filter(is.na(lon))
write.csv2(noCoord, "DataSource/nocoordPartners.csv")

ggcoordcorr <- read_delim("DataSource/nocoordPartnerscorrected.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
Partners <- Partners %>% 
  filter(!is.na(lon)) %>% 
  rbind(select(ggcoordcorr, -(X1)))


## Add ID partners and ID participations
Partners <- Partners %>% 
  rownames_to_column(., "ID") %>% 
  mutate(ID_PARTICIPATION = str_c("p", ID, sep = "")) %>% 
  select(., -(ID))

idPartners <- as_tibble(unique(Partners$Project.Partner))
idPartners <- idPartners %>% 
  rownames_to_column(., "id") %>% 
  mutate(ID_PARTNER = str_c("P", id, sep = ""), Project.Partner = value) %>% 
  select(., ID_PARTNER, Project.Partner)

Partners <- Partners %>% 
  left_join(x = Partners, y = idPartners, by = "Project.Partner")


## save Partners
#write.csv2(Partners, "DataSource/PartnersGeoCode.csv", row.names = F, fileEncoding = "UTF-8")




Partners <- read.csv2("AD/PartnersGeoCode.csv")


# check outside EU results
outEU <- Partners %>% 
  filter(lon < -14 | lon > 32 & lat < 35 | lat > 72)
outEUSP <- st_as_sf(outEU, coords = c("lon", "lat"), crs = 4326) %>% 
  st_transform(crs = 3035)
mapview(outEUSP)

## prepare data before re-geocoding outsiders
outEU <- outEU %>% 
  mutate(ShortLoc = str_c(tolower(Town), ", ", tolower(Country), sep = "")) 
outEU$ShortLoc <- gsub(" cedex", "", outEU$ShortLoc)

outEU <- outEU %>% 
  mutate(ShortLoc = ifelse(ID_PARTICIPATION == "p5913", c("israel"),
                           ifelse(ID_PARTICIPATION == "p26232", c("sweden"),
                                  ifelse(ID_PARTICIPATION == "p29992", c("norway"),
                           ShortLoc))))
                           
                                 
OutsiderLocation <- outEU$ShortLoc %>% unique()
is.na(OutsiderLocation)

## Geocoding participation places with gmap

register_google(key = "")

ggcoord_outsider <- ggmap::geocode(OutsiderLocation, output = "all")

## save output
#save(ggcoord_outsider, file = "AD/ggcoord_outsider.RDS")

str(ggcoord_outsider)
names(ggcoord_outsider)
length(ggcoord_outsider[[1]])


## re-format output
library(purrr)
library(magrittr)

results <- ggcoord_outsider %>% 
  map("results") 
str(results[1:3])

ad <- results %>%
  map(., 1) %>% 
  map_dfr(., magrittr::extract, "formatted_address")
loc <- results %>%
  map(., 1) %>%
  map("geometry") %>% 
  map("location") %>% 
  map_dfr(., magrittr::extract, c("lng", "lat"))
adloc <- cbind(ad, loc) %>% 
  transmute(formatted_address, new_lon = lng, new_lat = lat)

## filter na
ol <- as.data.frame(OutsiderLocation)
ol[63:67, 1] <- NA
ol[118, 1] <- NA
ol[212, 1] <- NA
ol[214, 1] <- NA
ol[216, 1] <- NA
ol[226:228, 1] <- NA
ol[236, 1] <- NA
ol[239, 1] <- NA
ol <- ol %>% 
  filter(!is.na(OutsiderLocation))
ol <- as.character(ol$OutsiderLocation)

## join
adloc$ShortLoc <- ol
outEU <- left_join(outEU, adloc, "ShortLoc")

## save
#save(outEU, file = "AD/outEU_newcoord.RDS")

## Pour Paul
outEU_nocoord <- outEU %>% 
  filter(is.na(new_lon))
#save(outEU_nocoord, file = "AD/outEU_nocoord.RDS")


Partners <- read.csv2("AD/PartnersGeoCode.csv")
load("AD/outEU_newcoord.RDS")
OutEUNoCoord <- read.csv2("AD/OutEU_nocoord_UTF8_corriges.csv")

# FINAL JOIN

outEU <- outEU %>% 
  filter(!is.na(new_lon)) %>% 
  rbind(OutEUNoCoord) 

Partners2 <- left_join(Partners, select (outEU, ID_PARTICIPATION, lon = new_lon, lat = new_lat),
                       by = "ID_PARTICIPATION")
Partners2 <- Partners2 %>% 
  filter(is.na(lon.y)) %>% 
  select(-lon.y, -lat.y) %>% 
  rename(lon = lon.x, lat = lat.x)

outEU2 <- outEU %>% 
  select(-lon, -lat, -ShortLoc, -formatted_address) %>% 
  mutate(lon = new_lon, lat = new_lat) %>% 
  select(-new_lon, -new_lat)

Partner <- rbind(Partners2, outEU2, by = "ID_PARTICIPATION")
Partner <- Partner %>% 
  filter(!is.na(ID_PARTICIPATION))

## save
save(Partner, file = "AD/Keep_ClosedProject_Partner_corrected.RDS")





