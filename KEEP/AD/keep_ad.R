###############################################################################
# NAME
# DESCRIPTION
# AUTHOR
# DATE
##############################################################################


setwd("C:/Users/Aurelie Douet/Desktop/these_paul/BD_Keep_Interreg/BD_Keep_Interreg")

library(readr)
library(tidyverse)
library(tidylog)
library(skimr)
#library(photon)
library(countrycode)
library(cartography)
library(sf)
library(ggmap)


# DataRep <- path.expand ('DataSource/')
# 
# list.files(DataRep)
# 
# Projects <- read.csv2("DataSource/Keep_ClosedProject_LeadPartner_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")
# 
# skim(Projects)
# 
# 

# Partners <- read.csv2("DataSource/Keep_ClosedProject_Partner2.csv", 
#                       stringsAsFactor = F, local = locale(encoding = "UTF-8"))
# guess_encoding("DataSource/Keep_ClosedProject_Partner.csv", 5000)

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

# join coord to the db
Partners <- left_join(x = Partners, y = select(ggcoord, Location, lon, lat), by = "Location")

##
noCoord <- Partners %>% 
  filter(is.na(lon))
write.csv2(noCoord, "DataSource/nocoordPartners.csv")


## Add coord manual corrected
ggcoordcorr <- read_delim("DataSource/nocoordPartnerscorrected.csv", 
                          ";", escape_double = FALSE, trim_ws = TRUE)
Partners <- Partners %>% 
  filter(!is.na(lon)) %>% 
  rbind(select(ggcoordcorr, -(X1)))

## Add id to partners and to participations
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
write.csv2(Partners, "DataSource/PartnersGeoCode.csv", row.names = F, fileEncoding = "UTF-8")