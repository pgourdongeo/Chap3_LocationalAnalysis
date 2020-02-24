
##==========================================================================##         
##                         INDICES DE DISPERSION                            ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base Eucicop/keep / analyse du semis de points 'locality'  ##
##               de la BD KEEP ; Calcul de l'indice de dispersion et        ##
##               des centres de gravités (point moyen et point médian)      ##
##               aux trois périodes                                         ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##

# CONTENTS
## 1. MAP - dotplot locality in large Europe
## 2. GRAPHIC - Dispersion index (R) - Fig. 3.8 
## 3. DIAGRAM - Centre de gravité


# Working directory huma-num
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




# ================ Prepare data ================


## Prepare data Localities

### Count nb particip/locality
sfParticipations_snap <- sfParticipations_snap %>% 
  group_by(geonameId) %>% 
  mutate(nbPart = n())


### Keep only unique locality
locality <- sfParticipations_snap %>% 
  filter(!duplicated(geonameId))


### Add ISO 
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
locality <- left_join(select(locality, geonameId, asciiName, ID_PARTNER, ID_PROJECT, Country = Country.y,
                             CityAddress, CountryISO, Period, nbPart), 
                      select(CORRESP_CNTR_ISO2, Country = COUNTRY, ISO = ISO_A2), 
                             by = "Country") 

#### Check
# # left_join: added 33 rows and added one column (ISO) ---> TN ???
# length(unique(locality$geonameId))
# bibi <- locality %>% filter(duplicated(geonameId))
# bibi2 <- locality %>% filter(is.na(ISO))
# rm(bibi, bibi2)

#### rm duplicated rows
locality <- locality %>% 
  filter(!duplicated(geonameId))



## Prepare sf Europe

### Europe Area for dispersion index (UE28 + Balkan, Suisse et Norway) = 37 countries
sfEUR <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Albania", "Bosnia and Herzegovina",
                                       "Kosovo", "Liechtenstein", "Montenegro", 
                                       "Republic of Macedonia", "Serbia", "Switzerland")) 

#mapView(sfEUR)

### Add ISO
sfEUR <- left_join(select(sfEUR, ID, NAME_EN), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), 
                   by = "NAME_EN")
sfEU <- left_join(select(sfEU, ID, NAME_EN),
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

sfEU <- sfEU %>% 
  mutate(Area = st_area(.)) %>% 
  group_by(ISO) %>% 
  mutate(AreaT = sum(Area)) %>% 
  ungroup()




# ==== 1. MAP - dotplot locality in large Europe ====

## FUNCTION : plot a points map
plot_points <- function(frame, adm, sf, title, source){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0))
  
  # Plot the map
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf), col = "#9E0E40", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)], add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = title, cex = 0.7, adj = 0, font = 2)
  
  # Add a layout
  layoutLayer(title = "",
              sources = source,
              author = "PG, AD, 2019",
              horiz = FALSE,
              col = NA,
              frame = F,
              scale = 500,
              posscale = c(6500000, 1000000)
  )
}

## data : points compris dans l'Europe large   
locality_inEU <- st_intersection(locality, select(sfEU, ID_POLY = ID, ISO_POLY = ISO))

## Display and save
pdf(file = "AD/OUT/dotplot_locality_0006.pdf", width = 8.3, height = 5.8)
plot_points(frame = rec, 
            adm = sfEU, 
            sf = locality_inEU %>% filter(Period == "2000-2006"),
            title = "Entre 2000 et 2006",
            source = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019")
dev.off() 




# ==== 2. GRAPHIC - Dispersion index (R) - Fig. 3.8 ====

## Filter le tableau de points avec les pays 
## pour lesquels on veut calculer les indices de dispersion
## UE28 + Balkan, Suisse et Norway = 37 countries
locality_inEU <- st_intersection(locality, select(sfEUR, ID_POLY = ID, ISO_POLY = ISO))
#mapView(sfEUR) + mapView(locality_inEU)


## Analyse de voisinage et processus de Poisson (cf. PUMAIN, ST-JULIEN)

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


## Compute mean dist for each period 

### Simple solution in dplyr
NNdistEur <- locality_inEU %>%
  group_by(Period) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )


### Prepare df 
NlocalityEUR <- locality_inEU %>% 
  group_by(Period) %>% 
  summarise(Npoints = n())
NlocalityEUR$geometry <- NULL
NNdistEur <- NNdistEur %>% 
  left_join(NlocalityEUR) %>%
  mutate(Area = sum(sfEUR2$Area))

### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistEur <- NNdistEur %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)

rm(NlocalityEUR)


## Compute mean dist for each period and each country

### Simple solution in dplyr
localityCNTR <- locality_inEU %>% 
  group_by(ISO_POLY, Period) %>% 
  mutate(N= n()) %>% 
  filter(N>1)
locality$N <- NULL
NNdistCountry <- localityCNTR %>%
  group_by(ISO_POLY, Period) %>%     # use ISO of EU shape 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )


### Prepare df 
Nlocality <- locality_inEU %>% 
  group_by(ISO_POLY, Period) %>% 
  summarise(Npoints = n())
NNdistCountry <- NNdistCountry %>% 
  left_join(Nlocality) %>% 
  left_join(sfEUR2, by = "ISO_POLY") %>% 
  as.data.frame() %>% 
  select(-geometry.x, -geometry.y)


### Calculate Re (estimated mean distance) and R index (gap between observed and estimated : Ra/Re)
### R = 0 -> concentration ; R = 1 -> Poisson random ; r > 1 -> dispersion 
NNdistCountry <- NNdistCountry %>%
  mutate(Area = as.numeric(Area),
         Re = 1/(2*sqrt(Npoints/Area)),
         R = Ra/Re)

rm(Nlocality, localityCNTR)


### ~Visualization ----
dfR <- NNdistCountry

# ### Members of EU since 2004 or after
# uePost <- c("CY", "EE", "HU", "LV", "LT", "MT", "PL", "SK", "SI", "CZ", "BG", "RO", "HR")
# ### UE-15 + suissse, Norvège
# ue15 <- c("DE", "BE", "FR", "IT", "LU", "NL", "DK", "IE", "GB", "GR", "ES", "PT", "AT", "FI", "SE", "CH", "NO")

### PG selection of countries 
selec <- c("FR", "IT", "ES", "FI", "SE", "LT", "LV", "BG", "RO", "CZ", "DE", "HU")

dfRselec <- dfR %>% filter(ISO_POLY %in% selec) %>% rename(UE_15 = ISO_POLY)

### Add european R index
NNdistEur<- NNdistEur %>% 
  mutate(UE_15 = "Europe") %>% 
  as.data.frame()
dfRselec <- rbind(dfRselec, NNdistEur)

### Fun colors
#library(ggthemes)
#scales::show_col(few_pal()(18))
#library(hrbrthemes)
#scales::show_col(ipsum_pal()(18))

### Need 13 colors
# library(ggsci)
# scales::show_col(pal_rickandmorty()(12))
# myPal <- c(pal_rickandmorty()(12), "black")

myPal <- c("#82491EFF", "#24325FFF", "#FB6467FF", 
           "#526E2DFF", "#E762D7FF", "#E89242FF",
           "#917C5DFF", "#69C8ECFF", "#d18975", 
           "#8fd175",  "#3f2d54", "#2d543d", "black")

dfRselec$UE_15 <- factor(dfRselec$UE_15, levels = c("FR", "IT", "ES", "FI", "SE", "LT", "LV", "BG", "RO", "CZ", "DE", "HU", "Europe"))


### Plot
Rindex <- ggplot(data = dfRselec, 
                 mapping = aes(x = Period, y = R, color= UE_15, group = UE_15)) +
  scale_color_manual("UE_15", values = myPal) +
  geom_point() +
  geom_line() + 
  labs(x = "",
       y = "Indice R") +
  geom_label_repel(data = dfRselec %>% filter(Period == "2014-2020"),
                   aes(label = UE_15), hjust = -0.2, size = 3) +
  scale_x_discrete(expand = expand_scale(add = .2)) +
  scale_y_continuous(breaks = seq(0.6, 1.8, 0.2)) +
  theme_light() +
  labs(caption = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ; ESPON DB 2013\nPG, AD, 2019") +
  theme(legend.position='none',
        plot.caption = element_text(size = 6))

### display and save
pdf(file = "AD/OUT/indiceR_locality.pdf", width = 8.3, height = 5.8)
Rindex
dev.off()  


### table abstract
dfRselecN <- dfRselec %>% 
  select(UE_15, Period, Npoints) %>% 
  spread(key = Period, value = Npoints, sep = "_N_", fill = 0)

colnames(dfRselecN) <- c("COUNTRY", "Npoints_0006", "Npoints_0713", "Npoints_1420")

dfRselecR <- dfRselec %>% 
  select(UE_15, Period, R) %>% 
  spread(key = Period, value = R)

colnames(dfRselecR) <- c("COUNTRY", "indiceR_0006", "indiceR_0713", "indiceR_1420")

myTble <- left_join(dfRselecN, dfRselecR)

myTble <- myTble %>% 
  mutate_at(.vars = c(5:7), all_vars(round(., 2)))



#### display and save
require(gridExtra)

pdf(file = "AD/OUT/indiceR_locality_tab.pdf", width = 8.3, height = 5.8)
grid.table(myTble, rows = NULL)
dev.off() 




# ==== 3. DIAGRAM - Centre de gravité ====


## data : points compris dans l'Europe large   
locality_inEU <- st_intersection(locality, select(sfEU, ID_POLY = ID, ISO_POLY = ISO))

## xy
coord <- as.data.frame(st_coordinates(locality_inEU))
dflocality <- locality_inEU %>% 
  mutate(x = coord$X, y = coord$Y) %>% 
  as.data.frame() %>% 
  select(-geometry) 

## Point moyen pondéré par le stock de participation
PG <- dflocality %>% 
  group_by(Period) %>% 
  summarise(., x = mean(x), y = mean(y),
            Mx_p = weighted.mean(x = x, y = nbPart),
            My_p = weighted.mean(x = y, y = nbPart)) %>% # pas de différence
  mutate(CG = "point moyen")


## Point médian
require(ICSNP)

df <- data.frame(x = numeric(3), y = numeric(3))
for(i in unique(dflocality$Period)){
  coords <- dflocality %>% filter(Period == i) %>% select(x, y)
  df[i, 1] <-spatial.median(coords)[1]
  df[i, 2] <-spatial.median(coords)[2]
}
df <- df[4:6, ]
df$Period <-  c("2000-2006", "2007-2013", "2014-2020")
df$CG <- "point médian"

## mean + median
PG <- rbind(select(PG, -Mx_p, -My_p), df)

## Clean envirmnt
rm(bibi, coord, coords, df, i)



## ~Visualization  ----

### diagram

#### prepare background map
#### country labels coords
label_coord <- data.frame(X =c(4400000, 4605000, 4605000),
                          Y = c(2875000, 2905000, 2790000),
                          NAME = c("ALLEMAGNE", "REP. TCHEQUE", "AUTRICHE"))
#### Main cities coords
cities <- locality %>% filter(Country == "Germany" | 
                                Country == "Austria" | 
                                Country == "Czech Republic") %>% ungroup()

coords <- as.data.frame(st_coordinates(cities))

dfcities <- cities %>% 
  mutate(X = coords$X, Y = coords$Y) %>% 
  as.data.frame() %>% 
  select(-geometry) 

dfcities <- dfcities %>% 
  filter(X > 4375000 & X < 4675000,
         Y > 2775000 & Y < 3012500) %>% 
  filter(!duplicated(asciiName)) %>% 
  filter(asciiName %in% c("Munich", "Nuernberg", "Passau", "Pizek", "Linz") | geonameId == "3068293") %>% 
  mutate(asciiName = ifelse(asciiName == "Nuernberg", "Nuremberg", asciiName))

sfcities <- st_as_sf(dfcities, coords = c("X", "Y"), crs = 3035) %>%
  st_sf(sf_column_name = "geometry") 

#### Create a scale bar
myScaleBar <- data.frame(X = c(4350000, 4375000),
                         Y = c(2800000, 2800000))

#### Plot
diagram <- ggplot()+ 
  geom_sf(data = sfEU, color = "ivory3", size = 0.5, fill = "ivory1") +
  geom_text(data = label_coord, aes(X, Y, label = NAME), size = 2) +
  geom_sf(data = sfcities, size = 1.5, fill = "black") +
  geom_sf_text(data = sfcities, aes(label = asciiName), size = 2, hjust = -0.2, vjust = -0.2) +
  coord_sf(xlim = c(4337500, 4712500), ylim =  c(2775000, 2962500), datum = sf::st_crs(3035), expand = FALSE) +
  #coord_sf(xlim = c(4337500, 4712500), ylim =  c(2775000, 3012500), datum = sf::st_crs(3035), expand = FALSE) +
  geom_line(data = PG, aes(x = x, y = y, color = CG, shape = CG), size = 0.8) + 
  geom_label_repel(data = PG, aes(x = x, y = y, label = Period), hjust = -0.2, vjust = -0.2, size = 2) +
  geom_point(data = PG, aes(x = x, y = y, color = CG, shape = CG), size = 2.5) +
  scale_color_manual(name = "Centre de gravité :", values = c("#E89242FF", "#526E2DFF")) +
  scale_shape_manual(values=c(17, 15)) +
  guides(shape = FALSE) +
  labs(x = "Coordonnée de X (en m)",
       y = "Coordonnée de Y (en m)") +
  annotate("text", label = "S/E", size = 3, x = 4700000, y = 2785000, color = "grey") +
  annotate("text", label = "N/O", size = 3, x = 4350000, y = 2950000, color = "grey") +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5) +
  annotate("text", label = "25 km", size = 2.5, x = 4357000, y = 2803000, hjust = 0) +
  theme_light() +
  labs(caption = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 / PG, AD, 2019") +
  theme(legend.position =  c(0.85, 0.85),
        plot.caption = element_text(size = 6))

#### display and save
pdf(file = "AD/OUT/gravity_locality_diagram.pdf", width = 8.3, height = 5.8)
diagram
dev.off() 

