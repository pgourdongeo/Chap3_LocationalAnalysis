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
library(dplyr)
library(readr)
library(mapview)
library(spdep)

# Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
#sfPartnerSpe <- st_read("AD/FDCARTE/sfPartner_3035_toGrid.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")

Partner2 <- read_delim("DataSource/PartnersIDProj.csv",
                       ";", escape_double = FALSE, locale = locale(),
                       trim_ws = TRUE)

Projects <- read.table("DataSource/ProjectsID.csv", 
                       sep=";", dec=".", 
                       #quote="",
                       header = TRUE,
                       encoding="UTF-8")

## Prepare data
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

sfPartPeriodSpe <- sfPartPeriod %>% 
  filter(!ID_PARTICIPATION %in% idvec) %>% 
  rbind(., sfPointsWater)


CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
sfPartPeriodSpe <- left_join(sfPartPeriodSpe, select(CORRESP_CNTR_ISO2, Country = COUNTRY, ISO_A2), by = "Country")



# PG
bibi <- as(sfPartPeriodSpe, "Spatial")
bibiM <- mean(bibi@coords)



# Europe Area (UE28 + Balkan, Suisse et Norway)
sfEUR <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Albania", "Bosnia and Herzegovina",
                                       "Kosovo", "Liechtenstein", "Montenegro", 
                                       "Republic of Macedonia", "Serbia", "Switzerland")) 
mapView(sfEUR)
sfEUR <- sfEUR %>% mutate(Area = st_area(.))
sfEUR <- left_join(sfEUR, select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), by = "NAME_EN")

length(unique(sfEUR$ISO))

# Faire un vecteur des pays d'intérêt
vecIso <- sort(unique(sfEUR$ISO))

## Filter le tableau de points avec les pays pour lesquels on veut calculer les indices de dispersion
#partners <- sfPartnerSpe %>% filter(ISO_A2 %in% vecIso)
partners <- sfPartPeriodSpe
partners <- st_intersection(sfPartPeriodSpe, select(sfEUR, ID, NAME_EN, ISO, Area))
mapView(sfEUR) + mapView(partners)

#### Calculer l'indice de dispersion sur l'ensemble des points
# transfo en spded

# DecaySP <- as(partners, "Spatial")
# listNearNei <- knearneigh(DecaySP@coords, k= 1, longlat = F)
# NearNeigh <- knn2nb(listNearNei)
# distnei <- nbdists(NearNeigh, DecaySP@coords, longlat = F)
# class(distnei)
# 
# distnei <- unlist(distnei)
# 
# summary(distnei)
# mean(distnei)


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

# Compute mean dist for each country and each period
NNdistCountry <- partners %>%
  group_by(ISO, Period) %>% 
  summarise(MeanDistNN(sf=., k = 1))


df <- list()
for(i in unique(partners$ISO)){
  
  sf1 <- partners %>% filter(ISO == i)
  
  MeanDist <- MeanDistNN(sf1, k=1)
  
  df[[i]] <- MeanDist
  
}

NNdistCountry <- cbind(unlist(df))


NPartners <- partners %>% group_by(ISO)%>% summarise(Ndecay = n())

NNdistCountry <- NNdistCountry %>% 
  as.data.frame() %>%
  mutate(ISO = rownames(.)) %>% 
  rename(MeanNNDist = V1)

NNdistCountry <- NNdistCountry %>% left_join(NPartners)
NNdistCountry <- NNdistCountry %>% left_join(select(sfEUR, ID, NAME_EN, ISO, Area), by = "ISO")
NNdistCountry <- NNdistCountry %>% as.data.frame()%>% select(-geometry.x,-geometry.y)

NNdistCountry <- NNdistCountry %>%mutate(Area = as.numeric(Area))   %>%mutate(Re = 1/(2*sqrt(Ndecay/Area)))
NNdistCountry <- NNdistCountry %>% mutate(R = MeanNNDist/Re)


