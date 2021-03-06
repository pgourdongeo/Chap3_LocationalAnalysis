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
partners <- st_intersection(sfPartPeriodSpe, select(sfEUR, ID_POLY = ID, ISO_POLY = ISO))
mapView(sfEUR) + mapView(partners)

#### Geocoding problem on partner table, example :
truc <- partners %>% filter(ID_PARTICIPATION == "p3325")
mapView(sfEUR) + mapView(truc)
rm(truc)


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

ind1 <- MeanDistNN(sf = partners %>% filter(Period == "2000-2006"), k= 1)
ind2 <- MeanDistNN(sf = partners %>% filter(Period == "2007-2013"), k= 1)
ind3 <- MeanDistNN(sf = partners %>% filter(Period == "2014-2020"), k= 1)

table(partners$Country)

#### Compute mean dist for each period and for all countries
##### Simple solution in dplyr
partnersE <- partners %>% group_by(Period) %>% mutate(N= n()) %>% filter(N>1)
partnersE$N <- NULL
NNdistEur <- partnersE %>%
  group_by(Period) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )

#### remove 6 points from sfPointsCorr not recording in bd Partner
NNdistEur <- na.omit(NNdistEur)

#### Prepare df 
NPartnersE <- partnersE %>% group_by(Period) %>% summarise(Npoints = n())
NPartnersE$geometry <- NULL
NNdistEur <- NNdistEur %>% 
  left_join(NPartnersE) %>%
  mutate(Area = sum(sfEUR2$Area))

#### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
#### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistEur <- NNdistEur %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)

rm(NPartnersE, partnersE)

#### Compute mean dist for each period and each country
##### Simple solution in dplyr
partners <- partners %>% group_by(ISO_POLY, Period) %>% mutate(N= n()) %>% filter(N>1)
partners$N <- NULL
NNdistCountry <- partners %>%
  group_by(ISO_POLY, Period) %>% # use ISO of EU shape 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )


##### loop that works
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


#### Prepare df 
NPartners <- partners %>% group_by(ISO_POLY, Period) %>% summarise(Npoints = n())
NNdistCountry <- NNdistCountry %>% 
  left_join(NPartners) %>% 
  left_join(sfEUR2, by = "ISO_POLY") %>% 
  as.data.frame() %>% 
  select(-geometry.x, -geometry.y) %>% 
  na.omit() # lost 5 points in Greece with no period

#### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
#### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistCountry <- NNdistCountry %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)



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
myPal <- c(ipsum_pal()(5), pal_rickandmorty()(12), "black")

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
  geom_label_repel(data = dfR15 %>% filter(Period == "2007-2013" & UE_15 == "LU" |
                                           Period == "2007-2013" & UE_15 == "PT"),
                   aes(label = UE_15), hjust = -0.2, size = 2) +
  theme_light() +
  theme(legend.position='none')

#### display and save
pdf(file = "AD/OUT/indiceR.pdf", width = 8.3, height = 5.8)
Rindex
dev.off()  




## Centre de gravité

### data
partners <- st_intersection(sfPartPeriodSpe, select(sfEUR, ID_POLY = ID, ISO_POLY = ISO))

### xy
coord <- as.data.frame(st_coordinates(partners))
dfPartners <- partners %>% 
  mutate(x = coord$X, y = coord$Y) %>% 
  as.data.frame() %>% 
  select(-geometry) 

### Point moyen
PG <- dfPartners %>% 
  group_by(Period) %>% 
  summarise(., Mx = mean(x), My = mean(y)) %>% 
  na.omit()


# ### Point médian : work but too long
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



### Point médian
library(ICSNP)

# Not elegant but works
df <- data.frame(Medx = numeric(3), Medy = numeric(3))
for(i in unique(PG$Period)){
  bibi <- dfPartners %>% filter(Period == i) %>% select(x, y)
  df[i, 1] <-spatial.median(bibi)[1]
  df[i, 2] <-spatial.median(bibi)[2]
}
df <- df[4:6, ]
PG <- cbind(PG, df)
PG$order <- c("1", "2", "3")

# Clean envirmnt
rm(bibi, coord, df)

#### Stock frame limits
bbrec <- st_bbox(rec)

#### Mapping centers of gravity

# Proposition 1
gravity1 <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
  geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
  coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
  geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  geom_point(data = PG,
             aes(x = Medx, y = Medy, shape = Period),
             color = "#78ba22", size = 2) +
  geom_line(data = PG,
            aes(x = Medx, y = Medy),
            color = "#78ba22", size = 0.4) +
  geom_point(data = PG,
             aes(x = Mx, y = My, shape = Period),
             color = "#69C8ECFF", size = 2) +
  geom_line(data = PG,
            aes(x = Mx, y = My),
            color = "#69C8ECFF", size = 0.4) +
  scale_shape_manual(values=c(16, 15, 17)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  labs(shape = "Proposition 1") +
  theme_void() +
  theme(legend.position = c(0.15, 0.6))

gravity1

# proposition 2
gravity2 <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
  geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
  coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
  geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  geom_point(data = PG,
             aes(x = Medx, y = Medy, color = Period),
             shape = 17, size = 2) +
  geom_point(data = PG, 
             aes(x = Mx, y = My, color = Period),
            shape = 15, size = 2) +
  scale_color_manual(values=c("#0E0B62", "#0F419F", "#187EDC")) +
  geom_sf(data = rec, color = "ivory4", fill = NA)+
  labs(color = "Proposition 2") +
  theme_void() +
  theme(legend.position = c(0.15, 0.6))
par(mar = c(0,0,0,0))
gravity2

#### Stock frame limits
bbeu <- st_bbox(sfEUR)
PG$shape <- c("Point médian")
PG2 <- PG %>% select(-Medx, -Medy) 
# proposition 2b
gravity2b <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
  geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
  coord_sf(xlim = bbeu[c(1,3)], ylim =  bbeu[c(2,4)], expand = FALSE) +
  geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  geom_point(data = PG,
             aes(x = Medx, y = Medy, color = Period, shape = shape),
             size = 2) +
  geom_point(data = PG2, 
             aes(x = Mx, y = My, color = Period, shape = shape),
             size = 2) +
  scale_shape_manual(values=c(17, 15)) +
  scale_color_manual(values=c("#0E0B62", "#0F419F", "#187EDC")) +
  #geom_sf(data = rec, color = "ivory4", fill = NA)+
  labs(color = "Localisation des centres de gravité en :",
       shape = "Centre de gravité") +
  theme_void() +
  theme(legend.position = c(0.25, 0.8), 
        panel.border = element_rect(color = "ivory4", fill = NA, size = 0.4))

gravity2b


# Proposition 3
gravity3 <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
  geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
  coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
  geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  geom_point(data = PG,
             aes(x = Medx, y = Medy),
             shape = 17, color = "black", size = 2) +
  geom_point(data = PG,
             aes(x = Mx, y = My),
             shape = 15, color = "black", size = 2) +
  geom_text(data = PG,
            aes(x = Medx, y = Medy, label = order), vjust = -0.5, hjust = -0.5, size = 2) +
  geom_text(data = PG,
            aes(x = Mx, y = My, label = order), vjust = -0.5, size = 2) +
  # geom_label_repel(data = PG,
  #                  aes(x = Mx, y = My, label = order), hjust = -0.2, size = 2) +
  geom_text(aes(x = 1500000, y = 3000000, label = "Proposition 3")) +
  geom_sf(data = rec, color = "ivory4", fill = NA)+
  theme_void() +
  theme(panel.background = element_rect(fill, colour, size, 
                                  linetype, color))

gravity3

c(1000000, 3000000)

# Brouillon
gravity <- ggplot() +
    theme_void() +
    geom_point(data = PG, aes(x = Mx, y = My, shape = Period), 
             color = "black", size = 2) +
    theme(legend.position = c(0.15, 0.6)) +
    geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF") +
    geom_sf(data = sfEUR, color = "ivory3", fill = "ivory4") +
    coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
    geom_point(data = dfPartners, 
             aes(x = x, y = y),
             colour = "#ff6208", size = 0.4) +
  # geom_point(data = PG,
  #            aes(x = Medx, y = Medy, color = Period),
  #            shape = 17, size = 2) +
  # scale_color_manual(values=c("#999999", "#E69F00", "#526E2DFF")) +
    geom_point(data = PG,
             aes(x = Medx, y = Medy, shape = Period),
             color = "#78ba22", size = 2) +
    geom_line(data = PG,
            aes(x = Medx, y = Medy),
            color = "#78ba22", size = 0.4) +
    geom_point(data = PG,
             aes(x = Mx, y = My, shape = Period),
             color = "#69C8ECFF", size = 2) +
    geom_line(data = PG,
            aes(x = Mx, y = My),
            color = "#69C8ECFF", size = 0.4) +
    # geom_point(data = PG, aes(x = Mx, y = My, shape = Period), 
    #           color = "black", size = 2) +
    scale_shape_manual(values=c(16, 15, 17)) +
    # geom_label_repel(data = PG,
    #                  aes(x = Mx, y = My, label = order), hjust = -0.2) +
    geom_sf(data = rec, color = "ivory4", fill = NA)


+
  #labs(shape = "Période") +
  theme_void() 

+
  theme(legend.position = c(0.15, 0.6))
   
gravity


  