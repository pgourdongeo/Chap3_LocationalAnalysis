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
library(ggplot2)
library(ggrepel)
library(ggsci)
library(tidylog)
library(ICSNP)


# Import data

# participations <- readRDS("Data/Participations_All_Eucicop.RDS")
# partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")
# projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")

## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")

sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")



## Prepare data Partners

### Count nb particip/partner
sfParticipations_snap <- sfParticipations_snap %>% 
  group_by(ID_PARTNER) %>% 
  mutate(nbPart = n())

partners <- sfParticipations_snap %>% 
  filter(!duplicated(ID_PARTNER))


### Add ISO 
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
partners <- left_join(select(partners, ID_PARTNER, ID_PROJECT, Country = Country.y,
                             CityAddress, CountryISO, geonameId, asciiName, Period, nbPart), 
                      select(CORRESP_CNTR_ISO2, Country = COUNTRY, ISO = ISO_A2), 
                             by = "Country") 

#### Check
# left_join: added 147 rows and added one column (ISO) ---> TN ???
length(unique(partners$ID_PARTNER))
bibi <- partners %>% filter(duplicated(ID_PARTNER))
bibi2 <- partners %>% filter(is.na(ISO))
rm(bibi, bibi2)

#### rm duplicated rows
partners <- partners %>% 
  filter(!duplicated(ID_PARTNER))




## Prepare sf Europe

### Europe Area (UE28 + Balkan, Suisse et Norway) = 37 countries
sfEUR <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Albania", "Bosnia and Herzegovina",
                                       "Kosovo", "Liechtenstein", "Montenegro", 
                                       "Republic of Macedonia", "Serbia", "Switzerland")) 

mapView(sfEUR)

### Add ISO
sfEUR <- left_join(select(sfEUR, ID, NAME_EN), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), 
                   by = "NAME_EN")

rm(CORRESP_CNTR_ISO2)

### Aggregate polygon of same country and sum area
sfEUR <- sfEUR %>% 
  mutate(Area = st_area(.)) %>% 
  group_by(ISO) %>% 
  mutate(AreaT = sum(Area)) %>% 
  ungroup()
sfEUR2 <- sfEUR %>% 
  select(ISO_POLY = ISO, Area = AreaT) %>% 
  filter(!duplicated(ISO_POLY))



## Dispersion index (R)

### Filter le tableau de points avec les pays 
### pour lesquels on veut calculer les indices de dispersion
partners_inEU <- st_intersection(partners, select(sfEUR, ID_POLY = ID, ISO_POLY = ISO))
mapView(sfEUR) + mapView(partners_inEU)


### Analyse de voisinage et processus de Poisson (cf. PUMAIN, ST-JULIEN)

#### Création d'une fonction dist moy au plus proche voisin (Ra)
MeanDistNN <- function(sf, k){
  
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  
  meanDist <- mean(distnei)
  
  return(meanDist)
  
}

MeanDistNN(sf = partners_inEU %>% filter(Period == "2000-2006"), k= 1) # = 1855 m
MeanDistNN(sf = partners_inEU %>% filter(Period == "2007-2013"), k= 1) # = 1406 m
MeanDistNN(sf = partners_inEU %>% filter(Period == "2014-2020"), k= 1) # = 5058 m

table(partners_inEU$Country)

#### Compute mean dist for each period and for all countries

##### Simple solution in dplyr
# partnersE <- partners_inEU %>% 
#   group_by(Period) %>% 
#   mutate(N= n()) %>% 
#   filter(N > 1)      # no need anymore
# partnersE$N <- NULL

NNdistEur <- partners_inEU %>%
  group_by(Period) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )


#### Prepare df 
NPartnersEUR <- partners_inEU %>% 
  group_by(Period) %>% 
  summarise(Npoints = n())
NPartnersEUR$geometry <- NULL
NNdistEur <- NNdistEur %>% 
  left_join(NPartnersEUR) %>%
  mutate(Area = sum(sfEUR2$Area))

#### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
#### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistEur <- NNdistEur %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)

rm(NPartnersEUR)


#### Compute mean dist for each period and each country

##### Simple solution in dplyr
partnersCNTR <- partners_inEU %>% 
  group_by(ISO_POLY, Period) %>% 
  mutate(N= n()) %>% 
  filter(N>1)
partners$N <- NULL
NNdistCountry <- partnersCNTR %>%
  group_by(ISO_POLY, Period) %>%     # use ISO of EU shape 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )


##### loop that works -----------------
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
# NNdistCountry <- NNdistCountry %>% 
#   as.data.frame() %>%
#   mutate(ISO = rownames(.)) %>% 
#   rename(MeanNNDist = V1)
##### end -----------------


#### Prepare df 
NPartners <- partners_inEU %>% 
  group_by(ISO_POLY, Period) %>% 
  summarise(Npoints = n())
NNdistCountry <- NNdistCountry %>% 
  left_join(NPartners) %>% 
  left_join(sfEUR2, by = "ISO_POLY") %>% 
  as.data.frame() %>% 
  select(-geometry.x, -geometry.y)


#### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
#### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistCountry <- NNdistCountry %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)

rm(NPartners, partnersCNTR)



### Visualization
dfR <- NNdistCountry

#### Members of EU since 2004 or after
uePost <- c("CY", "EE", "HU", "LV", "LT", "MT", "PL", "SK", "SI", "CZ", "BG", "RO", "HR")

#### UE-15 + suissse, Norvège
ue15 <- c("DE", "BE", "FR", "IT", "LU", "NL", "DK", "IE", "GB", "GR", "ES", "PT", "AT", "FI", "SE", "CH", "NO")
dfR15 <- dfR %>% filter(ISO_POLY %in% ue15) %>% rename(UE_15 = ISO_POLY)

#### Add european R index
NNdistEur<- NNdistEur %>% 
  mutate(UE_15 = "UE") %>% 
  as.data.frame()
dfR15 <- rbind(dfR15, NNdistEur)

#### Fun with colors
library(ggthemes)
scales::show_col(few_pal()(18))
library(hrbrthemes)
scales::show_col(ipsum_pal()(18))

#### Need 18 colors
library(ggsci)
scales::show_col(pal_rickandmorty()(18))
myPal <- c(pal_rickandmorty()(12), ipsum_pal()(5), "black")

#### Plot
Rindex <- ggplot(data = dfR15, 
                 mapping = aes(x = Period, y = R, color= UE_15, group = UE_15)) +
  scale_color_manual("UE_15", values = myPal) +
  geom_point() +
  geom_line() + 
  labs(x = "",
       y = "Indice R") +
  geom_label_repel(data = dfR15 %>% filter(Period == "2014-2020"),
                  aes(label = UE_15), hjust = -0.2, size = 2) +
  theme_light() +
  labs(caption = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ; ESPON DB 2013\nPG, AD, 2019") +
  theme(legend.position='none',
        plot.caption = element_text(size = 6))

#### display and save
pdf(file = "AD/OUT/indiceR.pdf", width = 8.3, height = 5.8)
Rindex
dev.off()  




## Centre de gravité

### data
partners_inEU <- st_intersection(partners, select(sfEUR, ID_POLY = ID, ISO_POLY = ISO))

### xy
coord <- as.data.frame(st_coordinates(partners_inEU))
dfPartners <- partners_inEU %>% 
  mutate(x = coord$X, y = coord$Y) %>% 
  as.data.frame() %>% 
  select(-geometry) 

### Point moyen pondéré par le stock de participation
PG <- dfPartners %>% 
  group_by(Period) %>% 
  summarise(., x = mean(x), y = mean(y),
               Mx_p = weighted.mean(x = x, y = nbPart),
               My_p = weighted.mean(x = y, y = nbPart)) %>% # pas de différence
  mutate(CG = "point moyen")

CG <- ggplot(PG)+
  geom_point(aes(x = x, y = y, color = CG, shape = CG), size = 2) +
  geom_line(aes(x = x, y = y, color = CG, shape = CG)) + 
  geom_label_repel(aes(x = x, y = y, label = Period), hjust = -0.2, vjust = -0.2, size = 2) +
  scale_color_manual(name = "Centre de gravité :", values = c("#E89242FF", "#526E2DFF")) +
  scale_shape_manual(values=c(17, 15)) +
  guides(shape = FALSE) +
  labs(x = "Coordonnée de X (en m)",
       y = "Coordonnée de Y (en m)") +
  annotate("text", label = "EST", size = 3, x = 4650000, y = 2750000) +
  #annotate("text", label = "OUEST", size = 3, x = 4400000, y = 2750000) +
  annotate("text", label = "NORD", size = 3, x = 4400000, y = 3000000) +
  theme_light() +
  theme(legend.position =  c(0.6, 0.8))

#### display and save
pdf(file = "AD/OUT/gravity_plot.pdf", width = 8.3, height = 5.8)
CG
dev.off() 

### Point médian : work but too long ---------------------------------------
# (cf. https://r.developpez.com/tutoriels/programmation-graphe/livre-R-et-espace/?page=chapitre-11-initiation-aux-statistiques-spatiales)
# bboxEur <- st_bbox(sfEUR)
# seqCoordX <- seq(bboxEur[1],
#                  bboxEur[3],
#                  by = 50000)
# 
# seqCoordY <- seq(bboxEur[2],
#                  bboxEur[4],
#                  by = 50000)
# 
# longGrid <- expand.grid(COORDX = seqCoordX,
#                         COORDY = seqCoordY)
# 
# #library(fields)
# medDist <- function(coord){
#   matDist <- rdist(coord, longGrid)
#   sumDist <- apply(matDist, 2, sum)
#   longGrid$ABSDIST <- sumDist
#   return(longGrid)
# }
# 
# 
# 
# longGrid1 <- medDist(coord = dfPartners %>% filter(Period == "2000-2006") %>% select(x, y))
# longGrid2 <- medDist(coord = dfPartners %>% filter(Period == "2007-2013") %>% select(x, y))
# longGrid3 <- medDist(coord = dfPartners %>% filter(Period == "2014-2020") %>% select(x, y))
# 
# bbrec <- st_bbox(rec)
# 
# ggplot() +
#   geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
#   geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
#   coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
#   geom_point(data = dfPartners, 
#              aes(x = x, y = y),
#              colour = "#ff6208", size = 0.5) +
#   geom_point(data = longGrid1[ which.min(longGrid1$ABSDIST),],
#              aes(x = COORDX, y = COORDY),
#              fill = "#526E2DFF", shape = 17, size = 2) +
#   geom_point(data = longGrid2[ which.min(longGrid2$ABSDIST),],
#              aes(x = COORDX, y = COORDY),
#              fill = "#526E2DFF", shape = 17, size = 2) +
#   geom_point(data = longGrid3[ which.min(longGrid3$ABSDIST),],
#              aes(x = COORDX, y = COORDY),
#              fill = "#526E2DFF", shape = 17, size = 2) +
#   geom_sf(data = rec, color = "ivory4", fill = NA) +
#   theme_void()
# 
### end ---------------------------------------


### Point médian
require(ICSNP)

df <- data.frame(x = numeric(3), y = numeric(3))
for(i in unique(dfPartners$Period)){
  bibi <- dfPartners %>% filter(Period == i) %>% select(x, y)
  df[i, 1] <-spatial.median(bibi)[1]
  df[i, 2] <-spatial.median(bibi)[2]
}
df <- df[4:6, ]
df$Period <-  c("2000-2006", "2007-2013", "2014-2020")
df$CG <- "point médian"

## prepare data for vizu
PG <- rbind(select(PG, -Mx_p, -My_p), df)

# Clean envirmnt
rm(bibi, coord, df)

#### Stock frame limits
bbrec <- st_bbox(rec)
bbeu <- st_bbox(sfEUR)

#### Mapping centers of gravity
gravity <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
  geom_sf(data = sfEUR, color = NA, fill = "ivory4") +
  geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  geom_sf(data = sfEUR, color = "ivory3", size = 0.2, fill = NA) +
  coord_sf(xlim = bbeu[c(1,3)], ylim =  bbeu[c(2,4)], expand = FALSE) +
  geom_point(data = PG,
             aes(x = x, y = y, color = Period, shape = CG),
             size = 2) +
  scale_shape_manual(values=c(17, 15)) +
  scale_color_manual(values=c("#0E0B62", "#0F419F", "#187EDC")) +
  #geom_sf(data = rec, color = "ivory4", fill = NA)+
  labs(color = "Localisation des centres de gravité en :",
       shape = "Centre de gravité") +
  theme_void() +
  labs(caption = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019") +
  theme(legend.position = c(0.22, 0.8), 
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9, hjust = 0),
        plot.caption = element_text(size = 6),
        panel.border = element_rect(color = "ivory4", fill = NA, size = 0.4))

#### display and save
pdf(file = "AD/OUT/gravity_map.pdf", width = 8.3, height = 5.8)
gravity
dev.off() 




### TRY TO COMBINE THE TWO PLOTS IN THE SAME PAGE

#### display and save
pdf(file = "AD/OUT/gravity_map_plot.pdf", width = 8.3, height = 5.8)
cowplot::plot_grid(gravity, CG_small,  
                   rel_widths = c(1, 1))
dev.off()


CG_small <- CG + 
  theme(axis.text = element_text(size = 6),
        legend.position = "none",
        axis.title = element_text(size = 7),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        panel.border = element_rect(color = "ivory4", fill = NA, size = 0.4))
#CG_small

#### display and save
pdf(file = "AD/OUT/gravity_map_plot_test.pdf", width = 8.3, height = 5.8)
ggdraw() +
  draw_plot(gravity, x = 0, y = 0, width = 1, height = 1) +
  draw_plot(CG_small, x = 0.5, y = 0.6, width = 0.45, height = 0.4)
dev.off()
