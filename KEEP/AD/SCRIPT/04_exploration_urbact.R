
##==========================================================================##         
##                       Exploration de la BD URBACT                        ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : BD EUCICOP-URBACT / réalisation des cartes et graphiques   ##
##               du chap.3                                                  ##
##                                                                          ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##


# CONTENTS
## 1. Headcount - Fig. 3.17
## 2. Fig. 3.19: Mapping ratio participations/city by country   
## 3. Barplot participations by typo NUTS U/R
## 4. Fig.3.22: Barplot participations by typo Nuts EF 2006-2013
## 5. Barplot participations by typo Nuts CP 2014-2020
## 6. Fig. 3.18: mapping nb participations URBACT by city
## 7. Fig. 3.19: planche participation by phase
## 8. Fig. 3.?: barplot nb leadPartner/ctry
## 9. Fig. 3.21: participation et classe de taille

# Working directory huma-num
# setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/PG_chap3/Chap3_LocationalAnalysis/KEEP")

# Library
library(readr)
library(sf)
library(mapview)
library(tidyverse)
library(tidylog)
library(cartography)
library(skimr)
library(ggplot2)


# Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035)

NUTS_CP_1420 <- st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
  st_transform(3035)

NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
  st_transform(3035)

## old
# urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
#                        ";", escape_double = FALSE, 
#                        col_types = cols(Code_Network = col_character()), 
#                        trim_ws = TRUE)

urbactCities <- read.csv2("AD/URBACT/UrbactDB_Vfinal/URBACT_Membership_GNidCorr_complete_V2.csv")
projet <- read.csv2("AD/URBACT/UrbactDB_Vfinal/UrbactNetworks_complete_V2.csv")


summary(projet$NbCities)


# ================ Prepare data ================

## Add phase to urbactCities
urbactCities <- urbactCities %>% 
  left_join(., select(projet, Code_Network, Phase))

## Number of city by network 
urbactCities <- urbactCities %>% 
  group_by(Code_Network) %>% 
  mutate(nbCity = length(unique(asciiName)))

summary(urbactCities$nbCity)


## aggregation: nb participation/city

## Prepare df
urbactCitiesAggr <- urbactCities %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(geonameId, asciiName, Lead, Country, Region, Continent, 
         lng_GN, lat_GN, nbCity) %>%
  group_by(geonameId) %>%
  mutate(NbPart = n(), NbLeader = sum(Lead)) %>% 
  select(-Lead) %>%
  distinct() %>%
  filter(!duplicated(geonameId)) %>% 
  filter(!is.na(lat_GN)) %>% 
  ungroup()

sum(urbactCitiesAggr$NbPart)
sum(urbactCitiesAggr$NbLeader)

sfUrbactCitiesAggr <- st_as_sf(urbactCitiesAggr, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035) 

mapview(sfEU) + mapview(sfUrbactCitiesAggr)

# ## villes qui participent le plus en espagne
# esp <- urbactCitiesAggr %>% filter(Country == "ES")
# topEsp <- esp %>%  ungroup() %>% top_n(wt = NbPart, n = 7)
# ## les projets qui ont lead partner en espagne
# espLead <- urbactCities %>% filter(City.Statut == "Lead Partner" & Country == "ES")


class(sfUrbactCitiesAggr)



# ================ 1. Headcount ================


## ----~barplot Nb participation/city ----

freq <- as.data.frame(table(urbactCitiesAggr$NbPart))

summary(urbactCitiesAggr$NbPart)

freq_nbpart <- ggplot(data = freq,
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = paste(Freq, "\nvilles", sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.42, color = "white", size = 4) +
  labs(x = "Nombre de participations par ville", 
       y = "Nombre de villes") +
  labs(caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  annotate("text", x = 4, y = 150, hjust = 0,
           label = str_c(sum(urbactCitiesAggr$NbPart), " participations\n", 
                         length(unique(urbactCitiesAggr$geonameId)), " villes")) +
  theme(plot.caption = element_text(size = 6))

## display end save
# pdf(file = "AD/OUT/freq_nbpartByCity_urbact.pdf", width = 8.3, height = 5.8)
# freq_nbpart
# dev.off()


## ----~barplot Nb participation/city% ----
freq <- freq %>% 
  mutate(pct = Freq / sum(Freq) * 100)

freq_pctPart <- ggplot(data = freq,
                      aes(x = reorder(Var1, -Freq), y = pct)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = paste(Freq, "\nvilles", sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.42, color = "white", size = 4) +
  labs(x = "Nombre de participations par ville", 
       y = "Pourcentage de villes impliquées") +
  labs(caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  annotate("text", x = 4, y = 30, hjust = 0,
           label = str_c(sum(urbactCitiesAggr$NbPart), " participations\n", 
                         length(unique(urbactCitiesAggr$geonameId)), " villes")) +
  theme(plot.caption = element_text(size = 6))

## display and save
# pdf(file = "AD/OUT/freq_pctpart_urbact.pdf", width = 8.3, height = 5.8)
# freq_pctPart
# dev.off()


## ----~Fig. 3.17: barplot Nb city/country ---- 

#Luxembourg is not in the network

freq2 <- as.data.frame(table(urbactCitiesAggr$Country)) %>% 
  mutate(pct = Freq / sum(Freq) * 100)

summary(freq2$Freq)

freq_NbVille <- ggplot(data = freq2,
       aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "Identity") +
  labs(x = "Pays impliqués dans des projets URBACT", 
       y = "Nombre de villes du programme URBACT") +
  labs(caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  annotate("text", x = 15, y = 40, hjust = 0,
           label = str_c(sum(freq2$Freq), " villes\n", 
                         nrow(freq2), " pays")) +
  theme(plot.caption = element_text(size = 6))


# display and save
pdf(file = "AD/OUT/freq_NbVille_urbact.pdf", width = 8.3, height = 5.8)
freq_NbVille
dev.off()




# ===== 2. Fig. 3.1?: Mapping ratio participations/city by country ======


## count
urbactCitiesAggr <- urbactCitiesAggr %>% 
  group_by(Country) %>% 
  mutate(RATIO = sum(NbPart)/length(asciiName)) %>% 
  ungroup()


## Prepare sf Europe
## UE28 + Suisse et Norway
sfEUR <- sfEU %>% 
  filter(UE28 == TRUE | NAME_EN %in% c("Norway", "Switzerland"))

### Add ISO
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
sfEUR <- left_join(select(sfEUR, ID, NAME_EN), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO = ISO_A2), 
                   by = "NAME_EN")

rm(CORRESP_CNTR_ISO2)

### UK -> GB
urbactCitiesAggr <- urbactCitiesAggr %>% 
  mutate(ISO = recode(Country, "UK" = "GB"))
urbactCntry <- urbactCitiesAggr %>% 
  group_by(ISO) %>% 
  slice(1)

### join RATIO to sfEUR
sfEUR <-  left_join(sfEUR, select(urbactCntry, ISO, RATIO), by = "ISO")

## Explo distribution
skim(sfEUR$RATIO) 

### distrib without extreme value (EE) - OLD
# distrib <- sfEUR %>% 
#   group_by(ISO) %>% 
#   slice(1) %>% 
#   filter(ISO != "EE")
# myVar <- sort(unique(distrib$RATIO))
# skim(myVar)
# hist(myVar)
# 
# ### distrib with extreme value (EE)
# distrib2 <- sfEUR %>% 
#   group_by(ISO) %>% 
#   slice(1) 
# myVar2 <- sort(unique(distrib2$RATIO))
# skim(myVar2) # mean = 1.99
# hist(myVar2)

### defines a set of breaks and colors
mybks <- c(min(sfEUR$RATIO), 
           mean(sfEUR$RATIO) - 1.5 * sd(sfEUR$RATIO), 
           mean(sfEUR$RATIO), 
           mean(sfEUR$RATIO) + 1.5 * sd(sfEUR$RATIO), 
           max(sfEUR$RATIO))

cols <- carto.pal("turquoise.pal", length(mybks))

## Create a "typo"" variable
sfEUR <- sfEUR %>%
  mutate(typo = cut(RATIO, breaks = mybks, dig.lab = 2, 
                    include.lowest = TRUE))

### Add extreme value to the typo and rewrite a pretty legend - OLD
# maxrm <- max(sfEUR$RATIO, na.rm = TRUE)
# extreme <- sfEUR %>% 
#   filter(RATIO == maxrm) %>% 
#   mutate(typo = recode(ISO, "EE" = "ext"))
# sfEUR <- rbind(sfEUR %>% filter(RATIO != maxrm | is.na(RATIO)), extreme)
# sfEUR <- sfEUR %>% 
#   mutate(TYPO = recode(typo,
#                        "NA" = "NA",
#                        "ext" = "4,5 (Estonie)",
#                         "[1,1.3]" = "1 - 1,3",
#                         "(1.3,1.9]" = "1,3 - 1,9",
#                         "(1.9,2.4]" = "1,9 - 2,4",
#                         "(2.4,2.5]" = "2,4 - 2,5"))

sort(unique(sfEUR$typo))
sfEUR <- sfEUR %>%
  mutate(TYPO = recode(typo,
                        "[1,1.4]" = "1 - 1,4",
                        "(1.4,2.5]" = "1,5 - 2,5",
                        "(2.5,3.7]" = "2,6 - 3,7",
                        "(3.7,5]" = "3,8 - 5"))

## Stock frame limits
bbrec <- st_bbox(rec)

## create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

## plot
ratioMap <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = sfEUR,
          aes(fill = TYPO), colour = "ivory3", size = 0.4) +
  scale_fill_manual(name = "Ratio\n(nb de participations /\nnb de villes participantes)*",
                    values = cols, na.value = "ivory4") +
  annotate("text", label = paste0("Ratio moyen* = ", round(mean(sfEUR$RATIO),1)), size = 3.5, hjust = 0,
           x = bbrec[1] + 270000, y = bbrec[2] + 2200000) +
  annotate("text", label = "*Discrétisation effectuée\nselon la moyenne et\n1,5 écart-type",
           x = bbrec[1] + 270000, y = bbrec[2] +1500000, size = 2.8, hjust = 0) +
  annotate("text", label = str_c(sum(urbactCitiesAggr$NbPart), " participations\n", 
                                 length(unique(urbactCitiesAggr$geonameId)), " villes"),
           size = 3,
           x = c(st_bbox(rec)[3]-1000000), y = c(st_bbox(rec)[4]-800000)) +
  annotate("text", label = "Source : EUCICOP-URBACT, 2019 / PG, AD, 2019",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.18, 0.7), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))
  

### display and save
# pdf(file = "AD/OUT/map_ratioPV_urbact.pdf", width = 8.3, height = 5.8)
# ratioMap
# dev.off()




# ===== 3. Barplot participations by typo NUTS U/R ======


## snap outsiders 

#### join city points to nuts to have outsiders
outsiders <- sfUrbactCitiesAggr %>% 
  st_join(., select(nutsUR, Nuts_Id)) %>% 
  filter(is.na(Nuts_Id))

#mapview(nutsUR) + mapview(outsiders)

#### function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
#### Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf
st_snap_points <-  function(x, y, max_dist) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

#### Apply function
require(purrr) # correction of the nuts shape topology
snap_outsiders <- st_snap_points(outsiders, compact(nutsUR$geometry), max_dist = 50000)

#### replace coords to outsiders 
outsiders$geometry <- snap_outsiders
outsiders <- outsiders %>%  select(-Nuts_Id)
#mapview(nutsUR) + mapview(outsiders)


#### join ousiders snaped to insiders
id <- outsiders$geonameId
sfUrbactCitiesAggr <- sfUrbactCitiesAggr %>% 
  filter(!geonameId %in% id) %>% 
  rbind(., outsiders)

#### verif
# outsiders <- sfUrbactCitiesAggr %>%
#   st_join(., select(nutsUR, Nuts_Id)) %>%
#   filter(is.na(Nuts_Id))
#mapview(nutsUR) + mapview(outsiders)

rm(id, snap_outsiders, outsiders, st_snap_points, iso)



## join city to nuts 

### first recode variable Typo7
nutsUR <- nutsUR %>% 
  mutate(Typo7_v2 = recode(Typo_7Clv2,
                           "4" = "Régions sous dominance\nd'une métropole",         
                           "6" = "Régions avec densité\nurbaine élevée",            
                           "5" = "Régions à majorité\nde villes moyennes",         
                           "7" = "Régions avec densité\nurbaine et rurale élevées",   
                           "1" = "Régions rurales\nsous influence métropolitaine",
                           "2" = "Régions rurales\navec villes petites et moyennes",
                           "3" = "Régions rurales isolées"))

urbact_typoUR <- st_join(sfUrbactCitiesAggr, select(nutsUR, Nuts_Id, Typo7_v2), 
                         join = st_intersects, left = TRUE)
#mapview(nutsUR) + mapview(urbact_typoUR)


### Ratio of involvments in URBACT Networks by type of regions
TabCroisUR <- urbact_typoUR %>% 
  group_by(Typo7_v2) %>%
  summarise(N = n(), "Ensemble des participations" = sum(NbPart), 
            "Participations des lead partners" = sum(NbLeader)) %>% 
  ungroup()

plotTabCroisUR <- TabCroisUR %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des lead partners") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% 
  ungroup()

plotTabCroisUR <- plotTabCroisUR %>% 
  mutate(Typo7_v2 = replace_na(Typo7_v2, "Hors typologie"))

plotTabCroisUR$Typo <- factor(plotTabCroisUR$Typo7_v2,
                              levels = c("Régions sous dominance\nd'une métropole",
                                         "Régions avec densité\nurbaine élevée",
                                         "Régions à majorité\nde villes moyennes",
                                         "Régions avec densité\nurbaine et rurale élevées",
                                         "Régions rurales\nsous influence métropolitaine",
                                         "Régions rurales\navec villes petites et moyennes",
                                         "Régions rurales isolées",
                                         "Hors typologie"))


#### Need 8 colors
library(ggsci)
scales::show_col(pal_rickandmorty()(18))
#myPal <- c("grey", pal_rickandmorty()(8))
#carto.pal.info()
# myPal <- c(carto.pal("blue.pal", 3),
#            carto.pal("brown.pal", 1),
#            carto.pal("green.pal", 4), "grey")

myPal <- c("#135D89", "#4D95BA", "#96D1EA", "#9F7C59",
           "#186D2E", "#36842E", "#7CB271", "grey")

typo7 <- ggplot(data = plotTabCroisUR, 
                aes(x = Typo, y = Ratio, fill = Typo)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x = "", 
       y = "Nombre moyen de participations URBACT par type de région") +
  scale_fill_manual(name = "Typologie urbain/rural\n des NUTS (2 & 3)", values = myPal) +
  theme_light() +
  labs(caption = "sources : EUCICOP, 2019 ; ESPON DB, 2013 / PG, AD, 2019", size = 3) +
  theme(legend.position = c(0.88, 0.7),
        plot.caption = element_text(vjust= 1.5, size = 6), 
        axis.text.x = element_blank(),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 10))

### display and save
pdf(file = "AD/OUT/barplot_typo7_nutsUR_urbact.pdf", width = 8.3, height = 5.8)
typo7
dev.off()




# ===== 4. Fig.3.22: Barplot participations by typo Nuts EF 2006-2013 ======


urbactCities0713 <- urbactCities %>% filter(Start > 2005 & Start < 2014)

## prepare data
urbactCities0713Aggr <- urbactCities0713 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(geonameId, asciiName, Lead, Country, Region, Continent, 
         lng_GN, lat_GN) %>%
  group_by(geonameId) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct() %>%
  filter(!duplicated(geonameId)) %>% 
  ungroup()

sfurbactCities0713 <- st_as_sf(urbactCities0713Aggr, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

#mapview(sfEU) + mapview(sfurbactCities0713)


## Join cities points to nuts
urbact_typoEF <- st_join(sfurbactCities0713, select(NUTS_EF_0613, NUTS_ID, TYPE), 
                         join = st_intersects, left = TRUE)

#mapview(NUTS_EF_0613) + mapview(urbact_typoEF)


### Percentage and ratio of involvments in URBACT Networks by type of regions (EF 06-13)
TabCroisEF <- urbact_typoEF %>%
  group_by(TYPE) %>%
  summarise(N= n(), 
            "Ensemble des participations" = sum(NbParticipation), 
            "Participations des lead partners" = sum(NbLeader)) %>%
  mutate(TYPE = as.character(TYPE)) %>% 
  ungroup()

TabCroisEF[3, 1] <- c("Hors typologie")

TabCroisEFPlot <- TabCroisEF %>%
  gather(key = "Type", value= "NB", 
         "Ensemble des participations", 
         "Participations Lead Partner") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% 
  filter(!TYPE == "NA") %>% 
  ungroup()

### %
ggplot(data = TabCroisEFPlot, 
       aes(x = reorder(TYPE,-Pct), y = Pct, fill = TYPE )) + 
  geom_bar(stat = "Identity", position = "dodge") +
  labs(title = "",
       x= "Eligibilité des régions aux Fonds structurels pour 2007-2013", 
       y = "Part dans l'ensemble des participations") +
  scale_fill_discrete(name="Type of Involvment",
                      breaks=c("nLeaderCity", "nParticipation"),
                      labels=c("As Lead Partner", "All Participations"))+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))

#### Need 5 colors
library(ggsci)
myPal <- c(pal_rickandmorty()(4), "grey")
scales::show_col(pal_rickandmorty()(18))
myPal <- c("#24325FFF", "#82491EFF", "grey", "#B7E4F9FF", "#FAFD7CFF")

TabCroisEFPlot <- TabCroisEFPlot %>% 
  mutate(TYPE = recode(TYPE, "Competitiveness and Employment" = "Competitiveness and\nEmployment"))
### ratio
typo0713urbact <- ggplot(data = TabCroisEFPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio, fill = TYPE)) + 
  geom_bar(stat = "Identity") + 
  scale_fill_manual(values = myPal) +
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Eligibilité des régions aux Fonds structurels pour 2007-2013", 
       y = "Nombre moyen de participations URBACT par type de région") +
  #scale_fill_manual(values = myPal) +
  theme_light() +
  labs(caption = "sources : EUCICOP, 2019 ; FSE \nPG, AD, 2019", size = 3) +
  theme(legend.position = "none",
        plot.caption = element_text(vjust= 1.5, size = 6),
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        legend.text = element_text(size = 8),
        strip.text = element_text(size=12, face = "bold"))

### display and save
pdf(file = "AD/OUT/barplot_typo_nuts0713_urbact.pdf", width = 8.3, height = 5.8)
typo0713urbact
dev.off()



# ===== 5. Barplot participations by typo Nuts CP 2014-2020 ======


urbactCities1420 <- urbactCities %>% filter(Start > 2013)

## prepare data
urbactCities1420Aggr <- urbactCities1420 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(geonameId, asciiName, Lead, Country, Region, Continent, 
         lng_GN, lat_GN) %>%
  group_by(geonameId) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct()%>%filter(!duplicated(geonameId)) %>% 
  filter(!is.na(lat_GN)) %>% 
  ungroup()

sfurbactCities1420 <- st_as_sf(urbactCities1420Aggr, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

## Join cities points to nuts
urbact_typoCP <- st_join(sfurbactCities1420, select(NUTS_CP_1420, NUTS_ID, TYPE),
                         join = st_intersects, left = TRUE)

### Percentage and ratio of involvments in URBACT Networks by type of regions (CP 14-20)
TabCroisCP <- urbact_typoCP %>%
  group_by(TYPE) %>%
  summarise(N= n(), 
            "Ensemble des participations" = sum(NbParticipation), 
            "Participations des lead partners" = sum(NbLeader))%>%
  mutate(TYPE = as.character(TYPE)) %>% 
  ungroup()

TabCroisCP[3, 1] <- "Hors typologie"

TabCroisCPPlot <- TabCroisCP %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des lead partners")%>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% filter(!TYPE == "NA") %>% 
  ungroup()

### %
ggplot(data = TabCroisCPPlot, 
       aes(x = reorder(TYPE, -Pct), y = Pct, fill = Type )) + 
  geom_bar(stat = "Identity", position = "dodge") +
  labs(title = "",
       x= "Structural Funds Eligibility 2014-2020", 
       y = "Percentage of total participation")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 1), 
        axis.text.x = element_text(size = 10)) +
  scale_fill_discrete(name = "Type of Involvment",
                      breaks = c("nLeaderCity", "nParticipation"),
                      labels = c("As Lead Partner", "All Participations")) +
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))

#### Need 4 colors
library(ggsci)
myPal <- c(pal_rickandmorty()(3), "grey")
scales::show_col(pal_rickandmorty()(18))
myPal <- c("grey", "#E762D7FF", "#526E2DFF", "#24325FFF")
### ratio
typo1420 <- ggplot(data = TabCroisCPPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio, fill = TYPE)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Eligibilité des régions aux Fonds structurels pour 2014-2020", 
       y = "Nombre moyen de participations URBACT par type de région") +
  scale_fill_manual(name = "Typologie du SFE",
                    #breaks = c("nLeaderCity", "nParticipation"),
                    #labels = c("As Lead Partner", "All Participations"),
                    values = myPal) +
  theme_light() +
  labs(caption = "sources : EUCICOP, 2019 ; CP \nPG, AD, 2019", size = 3) +
  theme(legend.position = "none",
        plot.caption = element_text(vjust= 1.5, size = 6),
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        legend.text = element_text(size = 8))


### display and save
pdf(file = "AD/OUT/barplot_typo_nuts1420_urbact.pdf", width = 8.3, height = 5.8)
typo1420
dev.off()




# ===== 6. Fig. 3.18: mapping nb participations URBACT by city ======


### with squared values (to emphase the differences of proportionnality)
sfUrbactCitiesAggr <- sfUrbactCitiesAggr %>%
  mutate(NbPart2 = NbPart*NbPart)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

### "Nom des villes : à partir de 7 participations":
summary(sfUrbactCitiesAggr$NbPart)
##4d4d4d

citiesUrbact <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = sfUrbactCitiesAggr %>% st_centroid(),
          mapping = aes(size = NbPart2), colour = "#D2019550", show.legend = NA) +
  geom_sf(data = sfUrbactCitiesAggr %>% st_centroid(),
          mapping = aes(size = NbPart2), shape = 1, colour = "#D20195", show.legend = NA) +
  scale_size(name = "Nombre de participations URBACT\npar ville (2003-2019)",
             breaks = c(400, 100, 36, 16, 1),
             labels = c("20", "10", "6", "4", "1"),
             range = c(0.5, 10)) +
  annotate("text", label = "Nom des villes : à partir de 7 participations", 
           size = 2.7, x = 1000000, y = 2800000, hjust = 0) +
  annotate("text", label = str_c(sum(sfUrbactCitiesAggr$NbPart), " participations\n", 
                                 length(unique(sfUrbactCitiesAggr$geonameId)), " villes"),
           size = 3,
           x = c(st_bbox(rec)[3]-1000000), y = c(st_bbox(rec)[4]-800000)) +
  annotate("text", label = "Source : EUCICOP-URBACT, 2019 / PG, AD, 2019",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_sf_text(data = sfUrbactCitiesAggr %>% filter(NbPart > 6),
               aes(label = asciiName), size = 2.2, color = "black",
               check_overlap = TRUE) +
  # geom_sf_text(data = sfUrbactCitiesAggr %>% filter(asciiName %in% c("Suceava")),  #OLD
  #              aes(label = asciiName), size = 2.2, color = "black",
  #              nudge_x = 10000,
  #              nudge_y = 10000,
  #              check_overlap = FALSE) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(st_bbox(rec)[3]-800000), y = c(st_bbox(rec)[2]+280000)) +
  geom_sf(data = rec, fill = NA, color = "ivory4", size = 0.5) +
  coord_sf(crs = 3035, datum = NA,
           xlim = st_bbox(rec)[c(1,3)],
           ylim = st_bbox(rec)[c(2,4)]) +
  theme_void() +
  theme(legend.position =  c(0.18, 0.60), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5))


## display end save
pdf(file = "AD/OUT/propCitiesUrbact2_new.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
citiesUrbact
dev.off()



# ===== 7. Fig. 3.19: participation par période ======

## Prepare df
urbactCitiesAggrP <- urbactCities %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(geonameId, asciiName, Lead, Country, Region, Continent, 
         lng_GN, lat_GN, nbCity, Phase) %>%
  group_by(geonameId, Phase) %>%
  mutate(NbPart = n(), NbLeader = sum(Lead)) %>% 
  select(-Lead) %>%
  distinct() %>%
  filter(!duplicated(geonameId)) %>% 
  filter(!is.na(lat_GN)) %>% 
  ungroup()

sum(urbactCitiesAggr$NbPart)
sum(urbactCitiesAggr$NbLeader)

sfUrbactCitiesAggrP <- st_as_sf(urbactCitiesAggrP, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035) 

summary(sfUrbactCitiesAggrP$NbPart)


## prepare nb participations and city by phase
np <- sfUrbactCitiesAggrP %>% 
  st_drop_geometry() %>% 
  group_by(Phase) %>% 
  summarise(np = sum(NbPart))

nv <- sfUrbactCitiesAggrP %>% 
  st_drop_geometry() %>% 
  group_by(Phase) %>% 
  summarise(nv = length(unique(geonameId)))

n <- np %>% left_join(., nv)

rm(np, nv)


## mapping
citiesUrbactP <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = sfUrbactCitiesAggrP %>% st_centroid(),
          mapping = aes(size = NbPart), colour = "#D2019550", show.legend = NA) +
  geom_sf(data = sfUrbactCitiesAggrP %>% st_centroid(),
          mapping = aes(size = NbPart), shape = 1, colour = "#D20195", show.legend = NA) +
  scale_size(name = "Nombre de participations URBACT par ville",
             breaks = c(9, 4, 2, 1),
             labels = c("9", "4", "2", "1"),
             range = c(0.5, 6)) +
  labs(x = NULL, y = NULL, 
       caption = "Source : EUCICOP-URBACT, 2019 / PG, AD, 2019") +
  facet_wrap(~Phase, nrow = 2) +
  
  geom_text(data = n, 
            mapping = aes(x = c(st_bbox(rec)[3]-1000000), 
                          y = c(st_bbox(rec)[4]-800000), 
                          label = paste0(np, " participations\n",
                                         nv, " villes")),
            size = 3) +

  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  
  annotate("text", label = "500 km", size = 1.8, color = "#333333", hjust = 0,
           x = c(st_bbox(rec)[3]-800000), y = c(st_bbox(rec)[2]+280000)) +
  
  geom_sf(data = rec, fill = NA, color = "ivory4", size = 0.5) +
  
  coord_sf(crs = 3035, datum = NA,
           xlim = st_bbox(rec)[c(1,3)],
           ylim = st_bbox(rec)[c(2,4)]) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_void() +
  theme(legend.position =  c(0.68, 0.36),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        strip.text.x = element_text(size = 12),
        plot.caption = element_text(size = 5.5, vjust = 10, hjust = 0.97))



## display end save
pdf(file = "AD/OUT/propCitiesUrbactP.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
citiesUrbactP
dev.off()




# ===== 8. Fig. 3.?: barplot nb leadPartner/ctry ======

### nb de leadPartners par pays
df <- urbactCitiesAggrP %>% 
  group_by(Country) %>% 
  summarise(nbL = sum(NbLeader))
df <- df %>% 
  filter(nbL>0)

plotLeadCtry <- ggplot(data = df,
                      aes(x = reorder(Country, -nbL), y = nbL)) +
  geom_bar(stat = "Identity") +
  # geom_text(aes(label = paste(nbL, "\nLead Partners", sep = "")), 
  #           position = position_dodge(0.9), 
  #           vjust = 1.42, color = "white", size = 4) +
  labs(y = "Nombre de participations Lead Partner",
       x = NULL) +
  labs(caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  annotate("text", x = 14, y = 30, hjust = 0,
           label = str_c(sum(df$nbL), " participations\n",
                         length(unique(df$Country)), " pays")) +
  theme(plot.caption = element_text(size = 6))

## display end save
pdf(file = "AD/OUT/nbLeadPbyCtry_urbact.pdf", width = 8.3, height = 5.8)
plotLeadCtry
dev.off()

rm(df, n)



# ===== 9. Fig. 3.21: participation et classe de taille ======



## load 
bdcity <- readRDS("../CITY/Data/DBCity_LauUmzFua.rds")

bdcity <- bdcity %>% 
  st_drop_geometry() %>% 
  select(geonameId, 12:22)

## join
pop <- sfUrbactCitiesAggr %>% 
  inner_join(., bdcity)

## classe
pop <- pop %>% 
  mutate(population = as.numeric(population),
         PopAdmin11 = ifelse(is.na(PopAdmin11), population, PopAdmin11),
         kpop11 = case_when(PopAdmin11<50000 ~ "4.Petite ville",
                            PopAdmin11>=50000 & PopAdmin11<150000 ~ "3.Ville moyenne",
                            PopAdmin11>=150000 & PopAdmin11<500000 ~ "2.Grande ville",
                            PopAdmin11>=500000 ~ "1.Très grande ville"))

table(pop$kpop11)


## calcul
popPart <- pop %>% 
  group_by(kpop11) %>% 
  summarise(nPart = sum(NbPart),
            nLead = sum(NbLeader)) %>% 
  ungroup %>% 
  mutate(PPart = nPart/sum(nPart)*100,
         PLead = nLead/sum(nLead)*100) %>% 
  st_drop_geometry()

popPartLong <- popPart %>% 
  select(-nPart, -nLead) %>% 
  pivot_longer(-kpop11, names_to = "var", values_to = "val") %>% 
  mutate(var = recode(var, "PLead" = "Participations Lead Partner", "PPart" = "Ensemble des participations"))

## palette
myPal <- c("#135D89", "#4D95BA", "#36842E", "#7CB271")

## annotate une seule facet
ann_text <- data.frame(kpop11 = "4.Petite ville", val = 25, lab = "Text",
                       var = factor("Participations Lead Partner",
                                    levels = c("Ensemble des participations","Participations Lead Partner")))


## plot
plotPopPart <- ggplot(data = popPartLong, aes(x = kpop11, y = val, fill = kpop11)) +
  geom_col() +
  facet_wrap(~var) +
  scale_fill_manual(name = "Classe de taille*\n(pop. admin. 2011)", values = myPal) +
  geom_text(data = ann_text, label = "*Seuils : 50k; 150k; 500k", size = 2.5, hjust = 0.8) +
  labs(x = "", y = "Pourcentage de participations",
       caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  theme(legend.position = c(0.9, 0.8),
        plot.caption = element_text(vjust= 1.5, size = 6), 
        axis.text.x = element_blank(),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 10),
        strip.text.x = element_text(size = 12, face = "bold"))




## display end save
pdf(file = "AD/OUT/nbPart_ktaille_urbact.pdf", width = 8.3, height = 5.8)
plotPopPart
dev.off()







