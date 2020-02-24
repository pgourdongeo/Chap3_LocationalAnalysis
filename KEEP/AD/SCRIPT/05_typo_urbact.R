
##==========================================================================##         
##                      TYPOLOGIE DES PROJETS URBACT                        ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base URBACT / préparation des variables en vue de la       ##
##               réalisation d'une typologie des projets (CAH)              ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##

# CONTENTS

# Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)

# Library
library(readr)
library(sf)
library(mapview)
library(tidyverse)
library(tidylog)
library(cartography)
library(skimr)
library(ggsn) # scalebar on maps



# Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(Code_Network = col_character()), 
                           trim_ws = TRUE)




# ==== Last try : new city size percentages, primacy index & coef of variation =====

## Creation of a new table of URBACT projects (with number of city in each network)
network <- urbactCities %>% 
  group_by(Code_Network) %>% 
  summarise(Ncities = n()) 


## % of cities by city size (category) in each project

### First, create a new vars: class of city size (1, 2, 3 ou 4) 
### si classe de taille validée, nommer ces classes (petite ville, moyenne, etc. par ex)
skim(urbactCities$POPLAU2_2015)
urbactCities <- urbactCities %>% 
  filter(!is.na(POPLAU2_2015)) %>% 
  mutate(KPOP = case_when(POPLAU2_2015 < 50000 ~ "1",
                          POPLAU2_2015 > 50000 & POPLAU2_2015 < 150000 ~ "2",
                          POPLAU2_2015 > 150000 & POPLAU2_2015 < 300000 ~ "3",
                          TRUE ~ "4"))

table(urbactCities$KPOP)

### Count number of cities in each category by network
kTY_city <- urbactCities %>% 
  group_by(Code_Network, KPOP) %>% 
  summarise(N_Kcity_byNw = n())

### Add number of cities in each network
kTY_city <- left_join(kTY_city, network, by = "Code_Network")

### % 
kTY_city <- kTY_city %>% 
  mutate(P_Kcity_byNw = round(N_Kcity_byNw / Ncities * 100))

kTY_city_spread <- kTY_city %>% 
  select(Code_Network, KPOP, P_Kcity_byNw) %>% 
  spread(key = KPOP, value = P_Kcity_byNw, sep = "")

network <- left_join(network, kTY_city_spread)           

rm(kTY_city_spread, kTY_city)

network <- network %>% 
  mutate_at(vars("KPOP1", "KPOP2", "KPOP3", "KPOP4"), 
            replace_na, 0)


## real primacy index (pop city 1 / pop city 2)
pI <- urbactCities %>% 
  group_by(Code_Network) %>% 
  top_n(n = 2, wt = POPLAU2_2015) %>% 
  arrange(Code_Network, POPLAU2_2015) %>% 
  mutate(primacy_index = max(POPLAU2_2015)/min(POPLAU2_2015)) %>% 
  filter(!duplicated(primacy_index))

network <- network %>% left_join(select(pI, Code_Network, primacy_index))
rm(pI)

## coef of variation on the pop
variance <- urbactCities %>% 
  group_by(Code_Network) %>% 
  mutate(ect = sd(POPLAU2_2015),
         moy = mean(POPLAU2_2015),
         coefVar = ect/moy) %>% 
  filter(!duplicated(Code_Network))

network <- network %>% left_join(select(variance, Code_Network, coefVar))
rm(variance)

## save for exploratR
write.csv2(network, "AD/URBACT/URBACTforCAH.csv", row.names = FALSE, fileEncoding = "UTF-8")



## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU





# ==== First try  =====


## Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(Code_Network = col_character()), 
                           trim_ws = TRUE)



## Creation of a new table of URBACT projects (with number of city in each network)
network <- urbactCities %>% 
  group_by(Code_Network) %>% 
  summarise(Ncities = n()) 


## % of cities by city size (category) in each project

### First, create a new vars: class of city size (1, 2, 3 ou 4) 
### si classe de taille validée, nommer ces classes (petite ville, moyenne, etc. par ex)
skim(urbactCities$POPLAU2_2015)
urbactCities <- urbactCities %>% 
  mutate(KPOP = case_when(POPLAU2_2015 < 50000 ~ "1",
                         POPLAU2_2015 > 50000 & POPLAU2_2015 < 200000 ~ "2",
                         POPLAU2_2015 > 200000 & POPLAU2_2015 < 500000 ~ "3",
                         TRUE ~ "4"),
         POP_LEAD = ifelse(City.Statut == "Lead Partner", POPLAU2_2015, NA),
         CITY_LEAD = ifelse(City.Statut == "Lead Partner", Name, NA)) %>% 
  group_by(Code_Network) %>% 
  mutate(Ncountry = length(unique(Country)))

table(urbactCities$KPOP)

### Count number of cities in each category by network
kTY_city <- urbactCities %>% 
  group_by(Code_Network, KPOP) %>% 
  summarise(N_Kcity_byNw = n())

### Add number of cities in each network
kTY_city <- left_join(kTY_city, network, by = "Code_Network")

### % 
kTY_city <- kTY_city %>% 
  mutate(P_Kcity_byNw = round(N_Kcity_byNw / Ncities * 100))

kTY_city_spread <- kTY_city %>% 
  select(Code_Network, KPOP, P_Kcity_byNw) %>% 
  spread(key = KPOP, value = P_Kcity_byNw, sep = "")

network <- left_join(network, kTY_city_spread)           

rm(kTY_city_spread, kTY_city)

network <- network %>% 
  mutate_at(vars("KPOP1", "KPOP2", "KPOP3", "KPOP4"), 
            replace_na, 0)


## Population of lead partner for each network
popLead <- urbactCities %>% 
  filter(!is.na(CITY_LEAD)) %>% 
  select(Code_Network, CITY_LEAD, POP_LEAD)

network <- network %>% 
  left_join(., popLead)
rm(popLead)

## le nb de pays concernés 
Ncountry <- urbactCities %>% 
  select(Code_Network, Ncountry) %>% 
  filter(!duplicated(Code_Network))

network <- network %>% left_join(Ncountry)

rm(Ncountry)

## l'area de la bonding box formée par le semis de point 
sfUrbactCities <- st_as_sf(urbactCities, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

vec <- unique(sfUrbactCities$Code_Network)
dfarea <- data.frame(Code_Network = character(87), area = double(87), row.names = vec)
for (i in vec) {
  bx <- sfUrbactCities %>% filter(Code_Network == i) %>% st_bbox()
  polyg <- st_as_sfc(st_bbox(bx))
  area <- st_area(polyg)
  dfarea[, 1] <- vec
  dfarea[i, 2] <- area
}

dfarea <- dfarea %>% 
  mutate(areaKM2 = area/1e+6)
rm(polyg, bx, area, i, vec)
network <- network %>% 
  left_join(., select(dfarea, Code_Network, areaKM2), by = "Code_Network")

rm(dfarea)



## centre de gravité : point moyen 
### Point moyen pondéré par la pop
coords3035 <- as.data.frame(st_coordinates(sfUrbactCities))
coords3035 <-  coords3035 %>% 
  mutate(Code_Network = sfUrbactCities$Code_Network,
         POPLAU2_2015 = sfUrbactCities$POPLAU2_2015)
PG <- coords3035 %>% 
  group_by(Code_Network) %>% 
  summarise(X = mean(X), Y = mean(Y),
         Xp = weighted.mean(x = X, y = POPLAU2_2015),
         Yp = weighted.mean(x = Y, y = POPLAU2_2015)) %>% 
  mutate(CG = "point moyen")

network <- network %>% left_join(select(PG, Code_Network, X_mean = X, Y_mean = Y))

### Point médian marche pas
require(ICSNP)

vec <- unique(coords3035$Code_Network)
df <- data.frame(Code_Network = vec, 
                 medX = numeric(87), medY = numeric(87), row.names = vec)
for(i in unique(coords3035$Code_Network)){
  coord <- coords3035 %>% filter(Code_Network == i) %>% select(X, Y)
  df[i, 2] <-spatial.median(coord)[1]
  df[i, 3] <-spatial.median(coord)[2]
}


# Clean envirmnt
rm(bibi, coord, df, i)

## et la distance moyenne ou médiane. 
require(spdep)
## fonction dist moy au plus proche voisin (Ra)
MeanDistNN <- function(sf, k){
  
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  
  meanDist <- mean(distnei)
  
  return(meanDist)
  
}

## compute for each network
NNdist <- sfUrbactCities %>%
  group_by(Code_Network) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )

network <- network %>% left_join(NNdist)


write.csv2(network, "AD/URBACT/reseauxUrbact.csv", row.names = FALSE, fileEncoding = "UTF-8")


#Après on pourrait faire un ACP, ou même une CAH direct 
#sur toutes ces grandeurs normalisées pour classifier les projets




