## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")

# Library
library(geonames) 
library(tidyverse)
library(readr)

options(geonamesUsername="adouet")
#options(geonamesHost="api.geonames.org")

# source(system.file("tests","testing.R",package="geonames"),echo=TRUE)

# GNTest <-GNsearch(name = sampleEtmun$Locality_Siege, style = "FULL", maxRows = "1", cities = "cities1000")
# GNcountryInfo("KE")


# Import db urbact
urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(Code_Network = col_character()), 
                           trim_ws = TRUE)

# and aggregate nb participation/city

## Prepare data
urbactCitiesAggr <- urbactCities %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(CodeCity, Name, X, Y, Country, Region.x, Continent.x, 
         POPLAU2_2015, ID_UMZ, Pop2011, Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbPart = n(), NbLeader = sum(Lead)) %>% 
  select(-Lead) %>%
  distinct() %>%
  filter(!duplicated(CodeCity))

## UK -> GB
urbactCitiesAggr <- urbactCitiesAggr %>% 
  ungroup() %>% 
  mutate(ISO = recode(Country, "UK" = "GB"))


# ## Test sur sample
# sampleCity <- sample_n(urbactCitiesAggr, 5)
# 
# NameLocality <- sampleCity$Name
# cntr <- sampleCity$ISO
# 
# GNplacename1 <- list()
# for(i in NameLocality){
#     GNplacename1[[i]] <- GNsearch(name = i, style = "FULL", 
#                                  maxRows = "1", cities = "cities1000")
# }
# GNdf1 <- bind_rows(GNplacename1, .id = "Name")
# # With ISO
# GNplacename <- list()
# for(i in NameLocality){
#   for (j in cntr) {
#     GNplacename[[i]] <- GNsearch(name = i, ISO = j, style = "FULL", 
#                                  maxRows = "1", cities = "cities1000")
#   }
# }
# GNdf <- bind_rows(GNplacename, .id = "Name")

## All urbact cities
NameLocality <- urbactCitiesAggr$Name
cntr <- urbactCitiesAggr$ISO

GNplacename <- list()
for(i in NameLocality){
  for (j in cntr) {
    GNplacename[[i]] <- GNsearch(name = i, ISO = j, style = "FULL", 
                                 maxRows = "1", cities = "cities1000")
  }
}

#saveRDS(GNplacename, file = "AD/GeoNames/GNplacename.rds")