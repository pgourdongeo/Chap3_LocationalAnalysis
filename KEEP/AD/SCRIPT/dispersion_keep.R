###############################################################################
#                                   INDICES DE DISPERSION
#                         analyse du semis de points de la BD KEEP
#
# DESCRIPTION :  
#
# PG, AD, Novembre 2019
################################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)

# library
library(sf)
library(tidyverse)
library(readr)
library(mapview)
library(spdep)

# Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")

Partner2 <- read_delim("DataSource/PartnersIDProj.csv",
                       ";", escape_double = FALSE, locale = locale(),
                       trim_ws = TRUE)

Projects <- read.table("DataSource/ProjectsID.csv", 
                       sep=";", dec=".", 
                       #quote="",
                       header = TRUE,
                       encoding="UTF-8")

## Prepare data Partners
### add period to participations
partPeriod <- left_join(x = select(Partner2, ID_PARTICIPATION, ID_PARTNER, ID_PROJECT, Country, lon, lat),
                        y = select(Projects, Period, ID_PROJECT),
                        by = "ID_PROJECT")

### tibble to sf
sfPartPeriod <- st_as_sf(partPeriod, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

### Replace points in water
sfPointsCorr <- st_read("AD/FDCARTE/sfPartner_inwaterGNutsUR.geojson")
idvec <- sfPointsCorr$ID_PARTICIPATION

sfPointsWater <- sfPartPeriod %>% 
  filter(ID_PARTICIPATION %in% idvec)
sfPointsWater$geometry <- NULL

sfPointsCorr <- left_join(select(sfPointsCorr, ID_PARTICIPATION),
                          sfPointsWater,
                          by = "ID_PARTICIPATION")

sfPartPeriodSpe <- sfPartPeriod %>% 
  filter(!ID_PARTICIPATION %in% idvec) %>% 
  rbind(., sfPointsCorr)

rm(sfPointsCorr, sfPointsWater, sfPartPeriod, partPeriod, idvec)

### Add ISO
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
sfPartPeriodSpe <- left_join(sfPartPeriodSpe, 
                             select(CORRESP_CNTR_ISO2, Country = COUNTRY, ISO = ISO_A2), 
                             by = "Country")

### Participation duplicated because table projets 
### could have several rows for one project (depending on the number of lead partners)
sfPartPeriodSpe <- sfPartPeriodSpe %>% filter(!duplicated(ID_PARTICIPATION))



## Prepare sf Europe
### Europe Area (UE28 + Balkan, Suisse et Norway)
sfEUR <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Albania", "Bosnia and Herzegovina",
                                       "Kosovo", "Liechtenstein", "Montenegro", 
                                       "Republic of Macedonia", "Serbia", "Switzerland")) 

#mapView(sfEUR)

### Add ISO
sfEUR <- left_join(select(sfEUR, ID, NAME_EN), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), 
                   by = "NAME_EN")

### Aggregate polygon of same country and sum area
sfEUR <- sfEUR %>% 
  mutate(Area = st_area(.)) %>% 
  group_by(ISO) %>% 
  mutate(AreaT = sum(Area)) %>% 
  ungroup()
sfEUR2 <- sfEUR %>% 
  select(ISO, Area = AreaT) %>% 
  filter(!duplicated(ISO))


# sfTest <- sfTest %>% mutate(Area = st_area(.))
# sfTest <- left_join(sfTest, select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), by = "NAME_EN")
# ### Aggregate polygon of same country and sum area
# sfTest2 <- sfTest %>% group_by(ISO) %>% mutate(AreaTot = sum(Area))
# sfTest2 <- sfTest2 %>% select(ISO, AreaTot)%>% filter(!duplicated(ISO))
# 
# mapView(sfEUR)
# sfEUR <- sfEUR %>% mutate(Area = st_area(.))

## Dispersion index

### Faire un vecteur des pays d'intérêt
#vecISO <- sort(unique(sfEUR$ISO))
### Filter le tableau de points avec les pays 
### pour lesquels on veut calculer les indices de dispersion
#partners <- sfPartPeriodSpe %>% filter(ISO %in% vecISO)
partners <- st_intersection(sfPartPeriodSpe, select(sfEUR, ID_POLY = ID))
mapView(sfEUR) + mapView(partners)

### Calculer l'indice de dispersion sur l'ensemble des points
### transfo en spded

#### Création d'une fonction dist moy au plus proche voisin
MeanDistNN <- function(sf, k){
  
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  
  meanDist <- mean(distnei)
  
  return(meanDist)
  
}


ind1 <- MeanDistNN(sf = partners %>% filter(Period == "2000-2006"), k= 1)
ind2 <- MeanDistNN(sf = partners %>% filter(Period == "2007-2013"), k= 1)
ind3 <- MeanDistNN(sf = partners %>% filter(Period == "2014-2020"), k= 1)

table(partners$Country)

### Compute mean dist for each country and each period
### Simple solution in dplyr
NNdistCountry <- partners %>%
  group_by(ISO, Period) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1)))


### loop that works
# df <- list()
# for(i in unique(partners$ISO)){
#   for(j in unique(partners$Period)){
#   sf1 <- partners %>% filter(ISO == i) %>% filter(Period == j)
# 
#   MeanDist <- MeanDistNN(sf1, k=1)
# 
#   df[[paste(i,j, sep = "_")]] <- MeanDist
#   }
# }
# NNdistCountry <- cbind(unlist(df))


NPartners <- partners %>% group_by(ISO, Period)%>% summarise(Npoints = n())

# NNdistCountry <- NNdistCountry %>% 
#   as.data.frame() %>%
#   mutate(ISO = rownames(.)) %>% 
#   rename(MeanNNDist = V1)

NNdistCountry <- NNdistCountry %>% left_join(NPartners) 
### Aggregate polygon of same country and sum area
sfTest2 <- sfTest %>% group_by(ISO) %>% mutate(AreaTot = sum(Area))
sfTest2 <- sfTest2 %>% select(ISO, AreaTot)%>% filter(!duplicated(ISO))

NNdistCountry <- NNdistCountry %>% left_join(sfTest2, by = "ISO" )
NNdistCountry <- NNdistCountry %>% as.data.frame()%>% select(-geometry.x,-geometry.y)

NNdistCountry <- NNdistCountry %>%mutate(AreaTot = as.numeric(AreaTot))   %>%mutate(Re = 1/(2*sqrt(Npoints/AreaTot)))
NNdistCountry <- NNdistCountry %>% rename(MeanNNDist = `MeanDistNN(sf = ., k = 1)` )
NNdistCountry <- NNdistCountry %>% mutate(R = MeanNNDist/Re)


