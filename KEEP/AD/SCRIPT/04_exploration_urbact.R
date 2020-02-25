
##==========================================================================##         
##                       Exploration de la BD URBACT                        ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION :         ##
##                       ##
##                       ##
##                       ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##


# CONTENTS
## 1. Headcount - Fig. 3.16
## 2. GRAPHIC - Dispersion index (R) - Fig. 3.8 
## 3. DIAGRAM - Centre de gravité


# Working directory huma-num
# setwd("~/BD_Keep_Interreg/KEEP")

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

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035)

NUTS_CP_1420 <- st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
  st_transform(3035)

NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
  st_transform(3035)

urbactCities <- read_delim("AD/URBACT/BdCitiesUrbact_Code_UMZ_LAU2.csv", 
                       ";", escape_double = FALSE, 
                       col_types = cols(Code_Network = col_character()), 
                       trim_ws = TRUE)


# ================ Prepare data ================

## Number of city by network 
urbactCities <- urbactCities %>% 
  group_by(Code_Network) %>% 
  mutate(nbCity = length(unique(Name)))

summary(urbactCities$nbCity)


## aggregation: nb participation/city

## Prepare df
urbactCitiesAggr <- urbactCities %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(CodeCity, Name, X, Y, Country, Region.x, Continent.x, 
         POPLAU2_2015, ID_UMZ, Pop2011, Lead, nbCity) %>%
  group_by(CodeCity) %>%
  mutate(NbPart = n(), NbLeader = sum(Lead)) %>% 
  select(-Lead) %>%
  distinct() %>%
  filter(!duplicated(CodeCity))

sum(urbactCitiesAggr$NbPart)
sum(urbactCitiesAggr$NbLeader)

sfUrbactCitiesAggr <- st_as_sf(urbactCitiesAggr, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

mapview(sfEU) + mapview(sfUrbactCitiesAggr)

# ## villes qui participent le plus en espagne
# esp <- urbactCitiesAggr %>% filter(Country == "ES")
# topEsp <- esp %>%  ungroup() %>% top_n(wt = NbPart, n = 7)
# ## les projets qui ont lead partner en espagne
# espLead <- urbactCities %>% filter(City.Statut == "Lead Partner" & Country == "ES")




# ================ 1. Headcount ================


## ----~barplot Nb participation/city ----

freq <- as.data.frame(table(urbactCitiesAggr$NbPart))

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
  annotate("text", x = 4, y = 150, label = "404 villes\n765 participations", hjust = 0) +
  theme(plot.caption = element_text(size = 6))

## display end save
pdf(file = "AD/OUT/freq_nbpartByCity_urbact.pdf", width = 8.3, height = 5.8)
freq_nbpart
dev.off()


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
  annotate("text", x = 4, y = 30, label = "404 villes\n765 participations", hjust = 0)+
  theme(plot.caption = element_text(size = 6))

## display and save
pdf(file = "AD/OUT/freq_pctpart_urbact.pdf", width = 8.3, height = 5.8)
freq_pctPart
dev.off()


## ----~Fig. 3.16 : barplot Nb city/country ---- 

#Luxembourg is not in the network

freq2 <- as.data.frame(table(urbactCitiesAggr$Country)) %>% 
  mutate(pct = Freq / sum(Freq) * 100)

summary(freq2$Freq)

freq_NbVille <- ggplot(data = freq2,
       aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "Identity") +
  labs(x = "Pays impliqués dans des projets URBACT", 
       y = "Nombre de villes du réseau URBACT") +
  labs(caption = "Sources : EUCICOP-URBACT 2019 / PG, AD, 2019") +
  theme_light() +
  annotate("text", x = 15, y = 40, label = "404 villes\n29 pays", hjust = 0) +
  theme(plot.caption = element_text(size = 6))


# display and save
pdf(file = "AD/OUT/freq_NbVille_urbact.pdf", width = 8.3, height = 5.8)
freq_NbVille
dev.off()


## reprendre ici -------
## ===== 2. MAP: Ratio participations/city by country ======
urbactCitiesAggr <- urbactCitiesAggr %>% 
  group_by(Country) %>% 
  mutate(RATIO = sum(NbPart)/length(Name)) %>% 
  ungroup()


### Prepare sf Europe
### UE28 + Suisse et Norway
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


### Explo distribution
skim(sfEUR$RATIO) 

#### distrib without extreme value (EE)
distrib <- sfEUR %>% 
  group_by(ISO) %>% 
  slice(1) %>% 
  filter(ISO != "EE")
myVar <- sort(unique(distrib$RATIO))
skim(myVar)
hist(myVar)

#### distrib with extreme value (EE)
distrib2 <- sfEUR %>% 
  group_by(ISO) %>% 
  slice(1) 
myVar2 <- sort(unique(distrib2$RATIO))
skim(myVar2) # mean = 1.98
hist(myVar2)

#### defines a set of breaks and colors
mybks <- c(min(myVar), 
           mean(myVar) - 1.5 * sd(myVar), 
           mean(myVar), 
           mean(myVar) + 1.5 * sd(myVar), 
           max(myVar))

cols <- cols <- carto.pal("turquoise.pal", length(mybks))

### Create a "typo"" variable
sfEUR <- sfEUR %>%
  mutate(typo = cut(RATIO, breaks = mybks, dig.lab = 2, 
                    include.lowest = TRUE))

### Add extreme value to the typo and rewrite a pretty legend 
maxrm <- max(sfEUR$RATIO, na.rm = TRUE)
extreme <- sfEUR %>% 
  filter(RATIO == maxrm) %>% 
  mutate(typo = recode(ISO, "EE" = "ext"))
sfEUR <- rbind(sfEUR %>% filter(RATIO != maxrm | is.na(RATIO)), extreme)
sfEUR <- sfEUR %>% 
  mutate(TYPO = recode(typo,
                       "NA" = "NA",
                       "ext" = "4,5 (Estonie)",
                        "[1,1.3]" = "1 - 1,3",
                        "(1.3,1.9]" = "1,3 - 1,9",
                        "(1.9,2.4]" = "1,9 - 2,4",
                        "(2.4,2.5]" = "2,4 - 2,5"))

### Stock frame limits
bbrec <- st_bbox(rec)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

### plot
superbeCarte <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = sfEUR,
          aes(fill = TYPO), colour = "ivory3", size = 0.4) +
  scale_fill_manual(name = "Ratio\n(nb de participations /\nnb de villes participantes)*",
                    values = cols, na.value = "ivory4") +
  annotate("text", label = "Ratio moyen* = 1.9", size = 3.5, hjust = 0,
           x = bbrec[1] + 270000, y = bbrec[2] + 2200000) +
  annotate("text", label = "*Discrétisation effectuée\nselon la moyenne et\n1/2 écart-type et calculée\nsans la valeur maximum",
           x = bbrec[1] + 270000, y = bbrec[2] +1500000, size = 2.8, hjust = 0) +
  annotate("text", label = str_c(sum(urbactCitiesAggr$NbPart), " participations\n", 
                                 length(unique(urbactCities$CodeCity)), " villes URBACT"),
           size = 3,
           x = c(st_bbox(rec)[3]-1000000), y = c(st_bbox(rec)[4]-800000)) +
  annotate("text", label = "Source : EUCICOP-URBACT, 2019\nPG, AD, 2019",
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
pdf(file = "AD/OUT/map_ratioPV_urbact.pdf", width = 8.3, height = 5.8)
superbeCarte
dev.off()



# NUTS U/R ------------------------

## Join cities points to nuts

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

### !!! points in water (Oslo, Helsinki ...) ---> NA/Hors typo in the plot
urbact_typoUR <- st_join(sfUrbactCitiesAggr, select(nutsUR, Nuts_Id, Typo7_v2), 
                         join = st_intersects, left = TRUE)
#mapview(nutsUR) + mapview(urbact_typoUR)


### Ratio of involvments in URBACT Networks by type of regions
TabCroisUR <- urbact_typoUR %>% 
  group_by(Typo7_v2) %>%
  summarise(N = n(), "Ensemble des participations" = sum(NbPart), 
            "Participations des lead partners" = sum(NbLeader))

plotTabCroisUR <- TabCroisUR %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des lead partners") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N)

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
       y = "Ratio (nb de participations / nb de types de région)") +
  scale_fill_manual(name = "Typologie urbain/rural\n des NUTS (2 & 3)", values = myPal) +
  theme_light() +
  labs(caption = "sources : EUCICOP, 2019 ; ESPON DB, 2013\nPG, AD, 2019", size = 3) +
  theme(legend.position = c(0.88, 0.7),
        plot.caption = element_text(vjust= 1.5, size = 6), 
        axis.text.x = element_blank(),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 10))

### display and save
#pdf(file = "AD/OUT/barplot_typo7_nutsUR_urbact.pdf", width = 8.3, height = 5.8)
typo7
dev.off()


# Nuts EF 2006-2013 -------------------------------
urbactCities0713 <- urbactCities %>% filter(Start.x < 2013)

## prepare data
urbactCities0713Aggr <- urbactCities0713 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(CodeCity, Name, X, Y, Country, Region.x, Continent.x,
         POPLAU2_2015, ID_UMZ, Pop2011, Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct() %>%
  filter(!duplicated(CodeCity))

sfurbactCities0713 <- st_as_sf(urbactCities0713Aggr, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)


mapview(sfEU) + mapview(sfurbactCities0713)

## Join cities points to nuts
urbact_typoEF <- st_join(sfurbactCities0713, select(NUTS_EF_0613, NUTS_ID, TYPE), 
                         join = st_intersects, left = TRUE)

mapview(NUTS_EF_0613) + mapview(urbact_typoEF)


### Percentage and ratio of involvments in URBACT Networks by type of regions (EF 06-13)
TabCroisEF <- urbact_typoEF %>%
  group_by(TYPE) %>%
  summarise(N= n(), 
            "Ensemble des participations" = sum(NbParticipation), 
            "Participations des lead partners" = sum(NbLeader)) %>%
  mutate(TYPE = as.character(TYPE))

TabCroisEF[3, 1] <- c("Hors typologie")

TabCroisEFPlot <- TabCroisEF %>%
  gather(key = "Type", value= "NB", 
         "Ensemble des participations", 
         "Participations des lead partners") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% 
  filter(!TYPE == "NA")

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
myPal <- c("#24325FFF",  "#82491EFF","#FAFD7CFF", "#B7E4F9FF", "grey")
### ratio
typo0713urbact <- ggplot(data = TabCroisEFPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Eligibilité des régions aux Fonds structurels pour 2007-2013", 
       y = "Ratio (nb de participations / nb de type de régions)") +
  #scale_fill_manual(values = myPal) +
  theme_light() +
  labs(caption = "sources : EUCICOP, 2019 ; FSE \nPG, AD, 2019", size = 3) +
  theme(legend.position = "none",
        plot.caption = element_text(vjust= 1.5, size = 6),
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        legend.text = element_text(size = 8))

### display and save
pdf(file = "AD/OUT/barplot_typo_nuts0713_urbact.pdf", width = 8.3, height = 5.8)
typo0713urbact
dev.off()


# Nuts CP ---------------------------
urbactCities1420 <- urbactCities %>% filter(Start.x >= 2013)

## prepare data
urbactCities1420Aggr <- urbactCities1420 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1, 0)) %>%
  select(CodeCity, Name, X, Y, Country, Region.x, Continent.x,
         POPLAU2_2015, ID_UMZ, Pop2011, Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct()%>%filter(!duplicated(CodeCity))

sfurbactCities1420 <- st_as_sf(urbactCities1420Aggr, coords = c("X", "Y"), crs = 4326) %>%
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
  mutate(TYPE = as.character(TYPE))

TabCroisCP[3, 1] <- "Hors typologie"

TabCroisCPPlot <- TabCroisCP %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des lead partners")%>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% filter(!TYPE == "NA")

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

### ratio
typo1420 <- ggplot(data = TabCroisCPPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Eligibilité des régions aux Fonds structurels pour 2014-2020", 
       y = "Ratio (nb de participations / nb de type de régions)") +
  # scale_fill_manual(name = "Typologie du SFE",
  #                   #breaks = c("nLeaderCity", "nParticipation"),
  #                   #labels = c("As Lead Partner", "All Participations"),
  #                   values = myPal) +
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




## MAP: Figure 3.17 –---------------
## Nombre total de participations aux projets URBACT (II & III) par ville, entre 2007
## et 2016.  


### with squared values (to emphase the differences of proportionnality)
sfUrbactCitiesAggr <- sfUrbactCitiesAggr %>%
  mutate(NbPart2 = NbPart*NbPart)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))


citiesUrbact <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = sfUrbactCitiesAggr %>% st_centroid(),
          mapping = aes(size = NbPart2), colour = "#D2019599", show.legend = NA) +
  scale_size(name = "Nombre de projets URBACT\npar ville (URBACT II & III)",
             breaks = c(81, 36, 16, 4),
             labels = c("9", "6", "4", "2"),
             range = c(0.5, 10)) +
  annotate("text", label = "Nom des villes : à partir de 4 participations", 
           size = 2.7, x = 1000000, y = 2800000, hjust = 0) +
  annotate("text", label = str_c(sum(sfUrbactCitiesAggr$NbPart), " participations\n", 
                                 length(unique(sfUrbactCitiesAggr$CodeCity)), " villes URBACT"),
           size = 3,
           x = c(st_bbox(rec)[3]-1000000), y = c(st_bbox(rec)[4]-800000)) +
  annotate("text", label = "Source : EUCICOP-URBACT, 2019\nPG, AD, 2019",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_sf_text(data = sfUrbactCitiesAggr %>% filter(NbPart > 3),
               aes(label = Name), size = 2.2, color = "#4d4d4d",
               check_overlap = TRUE) +
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
pdf(file = "AD/OUT/propCitiesUrbact2.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
citiesUrbact
dev.off()

