###############################################################################
#                         Exploration de la BD URBACT
#                         
#
# DESCRIPTION : 
#
# PG, AD, Novembre 2019
##############################################################################


## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")

# Library
library(readr)
library(sf)
library(mapview)
library(tidyverse)
library(cartography)
library(skimr)


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




# aggregation: nb participation/city

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

sfUrbactCitiesAggr <- st_as_sf(urbactCitiesAggr, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

mapview(sfEU) + mapview(sfUrbactCitiesAggr)

## Effectifs 
freq <- as.data.frame(table(urbactCitiesAggr$NbPart))
freq_nbpart <- ggplot(data = freq,
       aes(x = Var1, y = Freq)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = paste(Freq, "\nvilles", sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.42, color = "white", size = 4) +
  labs(x = "Nombre de participations par ville", 
       y = "Nombre de villes") +
  theme_light() +
  annotate("text", x = 4, y = 150, label = "404 villes\n765 participations", hjust = 0)

# display end save
pdf(file = "AD/OUT/freq_nbpart_urbact.pdf", width = 8.3, height = 5.8)
freq_nbpart
dev.off()


## Effectifs en %
freq <- freq %>% 
  mutate(pct = Freq / sum(Freq) * 100)

freq_pctPart <- ggplot(data = freq,
                      aes(x = Var1, y = pct)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = paste(Freq, "\nvilles", sep = "")), 
            position = position_dodge(0.9), 
            vjust = 1.42, color = "white", size = 4) +
  labs(x = "Nombre de participations par ville", 
       y = "Pourcentage de villes") +
  theme_light() +
  annotate("text", x = 4, y = 30, label = "404 villes\n765 participations", hjust = 0)

# display and save
pdf(file = "AD/OUT/freq_pctpart_urbact.pdf", width = 8.3, height = 5.8)
freq_pctPart
dev.off()

## Nb ville/country --- Luxembourg is not in the network
freq2 <- as.data.frame(table(urbactCitiesAggr$Country)) %>% 
  mutate(pct = Freq / sum(Freq) * 100)

freq_NbVille <- ggplot(data = freq2,
       aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "Identity") +
  labs(x = "Pays impliqués dans des projets URBACT", 
       y = "Nombre de villes du réseau URBACT") +
  theme_light() +
  annotate("text", x = 15, y = 40, label = "404 villes\n29 pays", hjust = 0)

# display and save
pdf(file = "AD/OUT/freq_NbVille_urbact.pdf", width = 8.3, height = 5.8)
freq_NbVille
dev.off()



## average participations/city by country - map
urbactCitiesAggr <- urbactCitiesAggr %>% 
  group_by(Country) %>% 
  mutate(Mp_c = sum(NbPart)/length(Name)) %>% 
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

### join Mp_c to sfEUR
sfEUR <-  left_join(sfEUR, select(urbactCntry, ISO, Mp_c), by = "ISO")


# ## defines a unique set of breaks 
# skim(sfEUR$Mp_c)
# bks <- getBreaks(v = sfEUR$Mp_c, method = "geom", nclass = 6)
# cols <- carto.pal("turquoise.pal", length(bks) - 1)
# # Create a "typo"" variable
# sfEUR <- sfEUR %>%
#   mutate(typo = cut(Mp_c, breaks = bks, dig.lab = 2,
#                     include.lowest = TRUE)) %>%
#   mutate(TYPO1 = recode(typo,
#                         "[1,1.3]" = "1 - 1,3",
#                         "(1.3,1.7]" = "1,3 - 1,7",
#                         "(1.7,2.1]" = "1,7 - 2,1",
#                         "(2.1,2.7]" = "2,1 - 2,7",
#                         "(2.7,3.5]" = "2,7 - 3,5",
#                         "(3.5,4.5]" = "2,7 - 3,5"),
#          TYPO2 = recode(typo,
#                         "[1,1.3]" = "1\n1,3",
#                         "(1.3,1.7]" = "\n1,7",
#                         "(1.7,2.1]" = "\n2,1",
#                         "(2.1,2.7]" = "\n2,7",
#                         "(2.7,3.5]" = "\n3,5",
#                         "(3.5,4.5]" = "\n3,5"))


### defines a set of breaks and colors
maxrm <- max(sfEUR$Mp_c,na.rm = T)
sfEUR2 <- sfEUR %>% filter(Mp_c != maxrm | is.na(Mp_c))
skim(sfEUR2$Mp_c)
bks2 <- getBreaks(v = sfEUR2$Mp_c, method = "quantile", nclass = 4)
mybks <- c(1, 1.5, 1.8, 2.1, 2.5)
cols <- carto.pal("turquoise.pal", length(mybks))

# Create a "typo"" variable
sfEUR2 <- sfEUR2 %>%
  mutate(typo = cut(Mp_c, breaks = mybks, dig.lab = 2, 
                    include.lowest = TRUE))
extreme <- sfEUR %>% 
  filter(Mp_c == maxrm) %>% 
  mutate(typo = recode(ISO, "EE" = "ext"))
sfEUR2 <- rbind(sfEUR2, extreme)
sfEUR2 <- sfEUR2 %>% 
  mutate(TYPO = recode(typo,
                       "NA" = "NA",
                       "ext" = "4,5 (Lettonie)",
                        "[1,1.5]" = "1 - 1,5",
                        "(1.5,1.8]" = "1,5 - 1,8",
                        "(1.8,2.1]" = "1,8 - 2,1",
                        "(2.1,2.5]" = "2,1 - 2,5"))

### Stock frame limits
bbrec <- st_bbox(rec)

### plot
superbeCarte <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#E3DEBF", size = 0.1) +
  geom_sf(data = sfEUR2,
          aes(fill = TYPO), colour = "ivory3", size = 0.1) +
  scale_fill_manual(name = "Ratio\n(nb de participations /\nnb de villes participantes)",
                    values = cols, na.value = "ivory4") +
  coord_sf(xlim = bbrec[c(1,3)], ylim =  bbrec[c(2,4)], expand = FALSE) +
  theme_void() +
  labs(caption = "source : \nPG, AD, 2019", size = 3) +
  theme(legend.position = c(0.1, 0.7),
        plot.caption = element_text(size = 6))


### display and save
pdf(file = "AD/OUT/superbeCarteAUtiliserDeTouteUrgence_urbact.pdf", width = 8.3, height = 5.8)
superbeCarte
dev.off()


# NUTS U/R ------------------------

## Join cities points to nuts

### first recode variable Urbain rural
nutsUR <- nutsUR %>% 
  mutate(Typo8 = recode(Typo_8Clv1, 
                        "1" = "Régions sous dominance\nd'une métropole",
                        "2" = "Régions avec densité\nurbain/rurale élevées",
                        "3" = "Régions avec densité\nurbaine élevée",
                        "4" = "Régions à majorité\nde villes moyennes",
                        "8" = "Régions rurales\nsous influence métropolitaine",
                        "5" = "Régions rurales\nsous influence de grandes villes",
                        "6" = "Régions rurales\navec villes petites et moyennes",
                        "7" = "Régions rurales isolées"))

### !!! points in water (Oslo, Helsinki ...)
urbact_typoUR <- st_join(sfUrbactCitiesAggr, select(nutsUR, Nuts_Id, Typo8), 
                         join = st_intersects, left = TRUE)
mapview(nutsUR) + mapview(urbact_typoUR)


### Ratio of involvments in URBACT Networks by type of regions
TabCroisUR <- urbact_typoUR %>% 
  group_by(Typo8) %>%
  summarise(N = n(), "Ensemble des participations" = sum(NbPart), 
            "Participations des leaders" = sum(NbLeader))

plotTabCroisUR <- TabCroisUR %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des leaders") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N)

plotTabCroisUR <- plotTabCroisUR %>% 
  mutate(Typo8 = replace_na(Typo8, "Hors typologie"))

#### Need 9 colors
library(ggsci)
myPal <- c("grey", pal_rickandmorty()(8))

ggplot(data = plotTabCroisUR, 
       aes(x = reorder(Typo8, -Ratio), y = Ratio, fill = Typo8)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x = "", 
       y = "Ratio (nb de participations/nb de types de région)") +
  scale_fill_manual(name = "Typologie urbain/rural\n des NUTS (2 & 3)", values = myPal) +
  # scale_fill_discrete(name = "Urban/Rural typology\n of NUTS (2 & 3)",
  #                     breaks = c("Sous dominance\n d'une métropole",
  #                                "Densité\n urbain/rural\n élevées",
  #                                "Densité\n urbaine élevée",
  #                                "Majorité\n de villes moyennes",
  #                                "Rurales\n sous influence\nmétropolitaine",
  #                                "Rurales\n sous influence\nde grandes villes",
  #                                "Rurales\n avec villes \npetites et moyennes",
  #                                "Régions rurales\nisolées",
  #                                "Outside Typology"),
  #                     labels= c("Sous dominance\n d'une métropole",
  #                               "Densité\n urbain/rural\n élevées",
  #                               "Densité\n urbaine élevée",
  #                               "Majorité\n de villes moyennes",
  #                               "Rurales\n sous influence\nmétropolitaine",
  #                               "Rurales\n sous influence\nde grandes villes",
  #                               "Rurales\n avec villes \npetites et moyennes",
  #                               "Régions rurales\nisolées",
  #                               "Outside Typology")) +
  theme_light() +
  theme(legend.position = c(0.9, 0.7),
        #plot.subtitle = element_text(vjust = 1), 
        #plot.caption = element_text(vjust = 1), 
        #axis.text = element_text(vjust = 0.25), 
        axis.text.x = element_text(size = 8, angle = 20, vjust = 0.8),
        #axis.title = element_text(size = 12), 
        #☻plot.title = element_text(size = 16), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 12))




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
            "Participations des leaders" = sum(NbLeader)) %>%
  mutate(TYPE = as.character(TYPE))

TabCroisEF[3, 1] <- c("Hors typologie")

TabCroisEFPlot <- TabCroisEF %>%
  gather(key = "Type", value= "NB", 
         "Ensemble des participations", 
         "Participations des leaders") %>%
  group_by(Type) %>%
  mutate(Pct = (NB/sum(NB)) * 100, Ratio = NB/N) %>% 
  filter(!TYPE == "NA")

### %
ggplot(data = TabCroisEFPlot, 
       aes(x = reorder(TYPE,-Pct), y = Pct, fill = TYPE )) + 
  geom_bar(stat = "Identity", position = "dodge") +
  labs(title = "",
       x= "Structural Funds Eligibility 2006-2013", 
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

### ratio
ggplot(data = TabCroisEFPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio, fill = TYPE)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Structural Funds Eligibility 2006-2013", 
       y = "Ratio (nb de participations/nb de type of regions)") +
  scale_fill_manual(values = myPal) +
  theme_light() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        legend.text = element_text(size = 8))




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
            "Participations des villes organisatrices" = sum(NbLeader))%>%
  mutate(TYPE = as.character(TYPE))

TabCroisCP[3, 1] <- "Hors typologie"

TabCroisCPPlot <- TabCroisCP %>%
  gather(key = "Type", value = "NB", 
         "Ensemble des participations", 
         "Participations des villes organisatrices")%>%
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
ggplot(data = TabCroisCPPlot, 
       aes(x = reorder(TYPE, -Ratio), y = Ratio, fill = TYPE)) + 
  geom_bar(stat = "Identity") + 
  facet_wrap(~Type, scales = "free") +
  labs(title = "",
       x= "Structural Funds Eligibility 2014-2020", 
       y = "Ratio (nb de participations/nb de type de regions)") +
  scale_fill_manual(name = "Typologie du SFE",
                    #breaks = c("nLeaderCity", "nParticipation"),
                    #labels = c("As Lead Partner", "All Participations"),
                    values = myPal) +
  theme_light() +
  theme(legend.position = c(0.8, 0.7),
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        legend.text = element_text(size = 8), 
        legend.title = element_text(size = 12))
      










