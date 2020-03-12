
##==========================================================================##         
##                         INDICES DE DISPERSION                            ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN /  Calcul de l'indice de dispersion             ##
##               des villes membres (Europe large et UE + Suisse + Norvege  ##
##                + Balkans)                                                ##        
##                                                                          ##
## PG, AD, mars 2020                                                        ##
##==========================================================================##


# !!!! Ctrl + Shift + O to see the results !!!!

# Working directory huma-num
# setwd("~/BD_Keep_Interreg/ETMUN")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)


library(tidylog)
library(tidyverse)
library(sf)
library(mapview)
library(spdep)



# Import data
etmun <- readRDS("../CITY/CorrectedDB/ETMUN_Membership_GNidCorr.RDS")
sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")
# ISO 
CORRESP_CNTR_ISO2 <- read_delim("../KEEP/AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)


# ---- Prepare data: cities in Europe with nb of members etmun ----

## rm na : removed 9 out of 17333 rows (<1%)
sfCitiesEur <- etmun %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

## transform to sf
sfCitiesEur <- st_as_sf(sfCitiesEur, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

## filter cities in Europe
sfCitiesEur <- st_intersection(sfCitiesEur, rec)

#mapview(sfCitiesEur)

## summarise members ETMUN by cities
sfCitiesEur <- sfCitiesEur %>% 
  group_by(geonameId, asciiName) %>% 
  summarise(nbMembers = n()) %>% 
  ungroup()


#---- Prepare sf Europe ----

## Europe Area for dispersion index (UE28 + Balkan, Suisse et Norway) = 37 countries
sfEU37 <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Albania", "Bosnia and Herzegovina",
                                       "Kosovo", "Liechtenstein", "Montenegro", 
                                       "Republic of Macedonia", "Serbia", "Switzerland")) 

#mapView(sfEU37)

## Add ISO
sfEU37 <- left_join(select(sfEU37, ID, NAME_EN), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), 
                   by = "NAME_EN")
sfEU <- left_join(select(sfEU, ID, NAME_EN),
                  select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2),
                  by = "NAME_EN")

rm(CORRESP_CNTR_ISO2)

## Aggregate polygon of same country and sum area
sfEU37 <- sfEU37 %>% 
  mutate(Area = st_area(.)) %>% 
  group_by(ISO) %>% 
  mutate(AreaT = sum(Area)) %>% 
  ungroup()
sfEU37_aggr <- sfEU37 %>% 
  select(ISO_POLY = ISO, Area = AreaT) %>% 
  filter(!duplicated(ISO_POLY))

sfEU <- sfEU %>% 
  mutate(Area = st_area(.)) %>% 
  group_by(ISO) %>% 
  mutate(AreaT = sum(Area)) %>% 
  ungroup()
sfEU_aggr <- sfEU %>% 
  select(ISO_POLY = ISO, Area = AreaT) %>% 
  filter(!duplicated(ISO_POLY))


#---- Indice de dispersion ----

## Analyse de voisinage et processus de Poisson (cf. PUMAIN, ST-JULIEN)
## Création d'une fonction dist moy au plus proche voisin (Ra)
MeanDistNN <- function(sf, k){
  
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  
  meanDist <- mean(distnei)
  
  return(meanDist)
  
}


## ----~UE28 + Balkan, Suisse et Norway = 37 countries ----

### Filter le tableau de points avec les pays 
### pour lesquels on veut calculer les indices de dispersion
cities_inEU37 <- st_intersection(sfCitiesEur, 
                                 select(sfEU37, ID_POLY = ID, ISO_POLY = ISO))
#mapView(sfEU37) + mapView(cities_inEU37)
#table(cities_inEU37$ISO_POLY)

### Ra
Ra <- MeanDistNN(sf = cities_inEU37, k= 1) # 7705 m

### Area of spatial extend
Area <- sum(sfEU37_aggr$Area)

### Total of points
Tpoints <- sum(cities_inEU37$nbMembers)

### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
Re <- 1/(2*sqrt(Tpoints/Area))
R <-  Ra/Re 

### ----~~result: R = 0.8487743 ----


## ----~Europe large ----

### Filter le tableau de points avec les pays 
### pour lesquels on veut calculer les indices de dispersion
cities_inEU <- st_intersection(sfCitiesEur, select(sfEU, ID_POLY = ID, ISO_POLY = ISO))

### Ra
Ra <- MeanDistNN(sf = cities_inEU, k= 1) # 9318 m

### Area of spatial extend
Area <- sum(sfEU_aggr$Area)

### Total of points
Tpoints <- sum(cities_inEU$nbMembers)

### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
Re <- 1/(2*sqrt(Tpoints/Area))
R <-  Ra/Re 

### ----~~result: R = 0.6320207 ----


