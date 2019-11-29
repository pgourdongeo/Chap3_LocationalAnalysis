###############################################################################
#                           TYPOLOGIE DES PROJETS URBACT
#                         
#
# DESCRIPTION : réalisation d'une typologie des projets URBACT en fonction 
# des villes participantes
#
# PG, AD, Novembre 2019
##############################################################################


# Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")

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

# nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035)
# NUTS_CP_1420 <- st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
#   st_transform(3035)
# NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
#   st_transform(3035)

urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
                           ";", escape_double = FALSE, 
                           col_types = cols(Code_Network = col_character()), 
                           trim_ws = TRUE)


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

sum(urbactCitiesAggr$NbPart)
sum(urbactCitiesAggr$NbLeader)

# sfUrbactCitiesAggr <- st_as_sf(urbactCitiesAggr, coords = c("X", "Y"), crs = 4326) %>%
#   st_sf(sf_column_name = "geometry") %>%
#   st_transform(crs = 3035)
# 
# mapview(sfEU) + mapview(sfUrbactCitiesAggr)


# Creation of a new table of URBACT projects

network <- urbactCities %>% 
  group_by(Code_Network) %>% 
  summarise(NcitiesByNw = n()) 

## % of cities by city size (category) in each project
# avec le pourcentage de villes par catégorie de taille, 

### create a new var = class of city size (1, 2, 3 ou 4) 
### si classe de taille validée, nommer ces classes (petite ville, moyenne, etc. par ex)
skim(urbactCities$POPLAU2_2015)
urbactCities <- urbactCities %>% 
  mutate(KTY_LAU = case_when(POPLAU2_2015 < 50000 ~ "1",
                         POPLAU2_2015 > 50000 & POPLAU2_2015 < 200000 ~ "2",
                         POPLAU2_2015 > 200000 & POPLAU2_2015 < 500000 ~ "3",
                         TRUE ~ "4"),
         POP_LEAD = ifelse(City.Statut == "Lead Partner", POPLAU2_2015, NA),
         CITY_LEAD = ifelse(City.Statut == "Lead Partner", Name, NA))

table(urbactCities$KTY_LAU)

### Count number of cities in each category by network
kTY_city <- urbactCities %>% 
  group_by(Code_Network, KTY_LAU) %>% 
  summarise(N_Kcity_byNw = n())



### Add number of cities in each network
kTY_city <- left_join(kTY_city, network, by = "Code_Network")

### % 
kTY_city <- kTY_city %>% 
  mutate(P_Kcity_byNw = round(N_Kcity_byNw / NcitiesByNw * 100))

kTY_city_spread <- kTY_city %>% 
  spread(key = KTY_LAU, value = P_Kcity_byNw, sep = "")
KTY1 <- kTY_city_spread %>% drop_na(KTY_LAU1) 
KTY2 <- kTY_city_spread %>% drop_na(KTY_LAU2)
KTY3 <- kTY_city_spread %>% drop_na(KTY_LAU3) 
KTY4 <- kTY_city_spread %>% drop_na(KTY_LAU4)  ### too long


network <- network %>% 
  left_join(select(KTY1, KTY_LAU1)) %>%
  left_join(select(KTY2, KTY_LAU2)) %>%
  left_join(select(KTY3, KTY_LAU3)) %>%
  left_join(select(KTY4, KTY_LAU4))            ### too long

rm(KTY1, KTY2, KTY3, KTY4, kTY_city_spread)

network <- network %>% 
  mutate_at(vars("KTY_LAU1", "KTY_LAU2", "KTY_LAU3", "KTY_LAU4"), 
            replace_na, 0)


# Population of lead partner of each network
popLead <- urbactCities %>% 
  filter(!is.na(CITY_LEAD)) %>% 
  select(Code_Network, CITY_LEAD, POP_LEAD)

network <- network %>% 
  left_join(., popLead)
rm(popLead)

#l'area de la bonding box formée par le semis de point, 
sfUrbactCities <- st_as_sf(urbactCities, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

vec <- unique(sfUrbactCities$Code_Network)
dfarea <- data.frame(Code_Network = vec, AREA_DOTPLOT = double(87))
for (i in vec) {
  bx <- sfUrbactCities %>% filter(Code_Network == i) %>% st_bbox()
  polyg <- st_as_sfc(st_bbox(bx))
  area <- st_area(polyg)
  dfarea[, 2] <- area
}


## re-format output
library(purrr)
library(magrittr)

results <- polyg %>% 
  map(., "1") %>% 
str(results[1:3])

ad <- results %>%
  map(., 1) %>% 
  map_dfr(., magrittr::extract, "formatted_address")


st_as_sf(sfParticipations_snap)

st_sf(sf_column_name = "geometry")
truc <- st_as_sf(polyg[1])

coord <- as.data.frame(st_coordinates(partners_inEU))




rm(polyg, bx, area, i, vec)
network <- network %>% 
  left_join(., dfarea)

rm(dfarea)

#le nb de pays concernés, 
#et la distance moyenne ou médiane. 
#Après on pourrait faire un ACP, ou même une CAH direct 
#sur toutes ces grandeurs normalisées pour classifier les projets




PartByCountry <- N %>% left_join(N_UMZCountry)

Participations_ByCountry <- Participations_ByCountry %>% filter(!Country =="NO")

Participations_ByCountry <- Participations_ByCountry %>% mutate(PctParticipationUMZ = (Countryparticipations/NUMZCountry)*100,
                                                                PctUrbactCitiesUMZ = (Ncities_Country/ NUMZCountry)*100)

Participations_ByCountry <- Participations_ByCountry%>% filter(!PctUrbactCitiesUMZ>100)

ggplot(Participations_ByCountry, aes(x = reorder(Country,-PctUrbactCitiesUMZ), y = PctUrbactCitiesUMZ )) + 
  geom_bar(stat = "Identity")+
  labs(title = "Share of cities involved in URBACT by urban morphological zones",
       x="Country", y = "Percentage Cities involved by UMZ")

#### CAH
sumStat_resLAU <- urbactCities %>% 
  group_by(Code_Network) %>% 
  summarise(Mean_pop15 = mean(POPLAU2_2015), 
            Med_pop15 = median(POPLAU2_2015),
            Var_pop15 = var(POPLAU2_2015),
            Sd_pop15 = sd(POPLAU2_2015),
            Cv_pop15 = Sd_pop15/Mean_pop15,
            Min_pop15 = min(POPLAU2_2015),
            Max_pop15 = max(POPLAU2_2015)) %>% 
  as.data.frame() %>% 
  na.omit()

row.names(sumStat_resLAU) <- sumStat_resLAU[,1]
sumStat_resLAU <- sumStat_resLAU[,-1]

library(GGally)
ggpairs(sumStat_resLAU)
sumStat_resLAU <- sumStat_resLAU %>% select(-Sd_pop15,-Max_pop15)
library(cluster)
CAH <- agnes(x = sumStat_resLAU, metric="euclidean", method = "average")
plot(as.dendrogram(CAH), leaflab = "none")

sumStat_resLAU$CAH <- cutree(tree= CAH, k = 6)

sumMeanCAH <- sumStat_resLAU %>% 
  ungroup() %>% 
  group_by(CAH) %>%
  summarise(N = n(),
            Mean = mean(Mean_pop15),
            Med = mean(Med_pop15),
            Var = mean(Var_pop15),
            #Sd = mean(SdUMZ),
            Cv = mean(Cv_pop15),
            Min = mean(Min_pop15)
            # Max = mean(MaxUMZ))
  )


