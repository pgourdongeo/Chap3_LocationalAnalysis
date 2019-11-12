###############################################################################
#                               explo nuts
#
# DESCRIPTION : 
# 
# PG, AD, Novembre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")
setwd("~/git/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)


# library
library(dplyr)
library(sf)
library(tidylog)
library(skimr)
library(lwgeom)


# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

sfEU <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035) %>% 
  st_make_valid()

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

NUTS_CP_1420 <- st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
  st_transform(3035)

NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
  st_transform(3035)


par(mar = c(0, 0, 0, 0)) 
plot(st_geometry(sfEU))
plot(st_geometry(NUTS_EF_0613), add = TRUE)

par(mar = c(0, 0, 0, 0)) 
plot(st_geometry(sfEU))
plot(st_geometry(NUTS_CP_1420), add = TRUE)
