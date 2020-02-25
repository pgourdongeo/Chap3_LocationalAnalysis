##==========================================================================##         
##            Aggrégation de la base ville (GN) avec une jointure spatiale  ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base City (compilation ETMUN/EUCICOP/ URBACT)              ##
##               au niveau des localités                                    ##
##                                                                          ##
## PG, Fev 2020                                                             ##
##==========================================================================##


# CONTENTS
# 1. Load Data
# 2. Spatial Joint   
# 3. Aggregate GN in same Admin polygon (keep the most populated Populated Place)

##Packages
library(tidyverse)
library(sf)
library(tidylog)
library(readr)
library(mapview)


# Working directory huma-num
setwd("~/BD_Keep_Interreg/CITY/")


AdminEU <- readRDS("Data/AdminDelimPop0611.RDS")

GNall <- readRDS("Data/DBCity.rds")

GNallsf <- st_as_sf(GNall, coords = c("lng_GN", "lat_GN") , crs = 4326)%>% st_transform(crs= 3035)

### Subset Paris for test


ParisSf <- AdminEU %>% filter(COMM_ID== "FR1175056")

GNParis <- GNallsf %>% filter(str_detect(asciiName, "Paris"))


Gninter <- st_within(GNParis, ParisSf, sparse = T)
Gninter[2]
GNParis %>% split(rownames %in% Gninter)
