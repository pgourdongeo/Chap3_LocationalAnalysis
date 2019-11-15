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


# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfEU <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035) %>% 
  st_make_valid()

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")



# Un fichier point
DecaySmallUMZ2011 <- SmallUMZPoints %>% filter(TrajPopclusters == 3)%>% ungroup()

## Un fichier europe (si possible faire une sous selection de pays)
EuropeArea <- Europe %>% mutate(Area = st_area(.))

##### Faire un vecteur des pays d'intÃ©rÃªt (possiblement les mÃªmes que pour les liu)

vectorCOuntry <- c("DE","IT","UK","FR","PL","ES","NL","RO","CZ","HU","PT","BG","SE","AT","DK","BE")
## Filter le tableau de points avec les pays pour lesquels on veut calculer les indices de dispersion
DecaySmallUMZ2011 <- DecaySmallUMZ2011 %>% filter(Country %in% vectorCOuntry)

#### Calculer l'indice de dispersion sur l'ensemble des points
# transfo en spded
library(spdep)
DecaySP <- as(DecaySmallUMZ2011, "Spatial")


listNearNei <- knearneigh(DecaySP@coords, k= 1, longlat = F)

NearNeigh <- knn2nb(listNearNei)

distnei <- nbdists(NearNeigh, DecaySP@coords, longlat = F)
class(distnei)

distnei <- unlist(distnei)

summary(distnei)
mean(distnei)


#### CrÃ©ation d'une fonction dist moy au plus proche voisin


MeanDistNN <- function(sf, k){
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  meanDist <- mean(distnei)
  return(meanDist)
}

MeanDistNN(sf = DecaySmallUMZ2011, k= 1)

table(DecaySmallUMZ2011$Country)
### Compute mean dist for each country


NNdistCountry <- DecaySmallUMZ2011 %>% group_by(Country) %>% summarise(MeanDistNN(sf=., k = 1))





df <- list()
for(i in unique(DecaySmallUMZ2011$Country)){
  
  sf1 <- DecaySmallUMZ2011 %>% filter(Country == i)
  
  MeanDist <- MeanDistNN(sf1, k=1)
  
  df[[i]] <- MeanDist
  
  
}
NNdistCountry <- cbind(unlist(df))


NSmallUMZCountry <- DecaySmallUMZ2011 %>% group_by(Country)%>% summarise(NdecaySmall = n())

NNdistCountry <- NNdistCountry %>% as.data.frame() %>%mutate(Country = rownames(.))%>% rename(MeanNNDist = V1)

NNdistCountry <- NNdistCountry %>% left_join(NSmallUMZCountry)
NNdistCountry <- NNdistCountry %>% left_join(EuropeArea, by = c("Country"="ID"))
NNdistCountry <- NNdistCountry %>% as.data.frame()%>% select(-geometry.x,-geometry.y)

NNdistCountry <- NNdistCountry %>%mutate(Area = as.numeric(Area))   %>%mutate(Re = 1/(2*sqrt(NdecaySmall/Area)))
NNdistCountry <- NNdistCountry %>% mutate(R = MeanNNDist/Re)