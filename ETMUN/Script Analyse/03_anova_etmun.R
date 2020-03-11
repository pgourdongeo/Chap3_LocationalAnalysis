
##==========================================================================##         
##            ANALYSE UNI et BIVARIEE : VILLES ET MEMBRES ETMUN             ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN / régression linéaire et                        ##
##               cartographie des résidus                                   ##
##                                                                          ##
## PG, AD, janvier 2020                                                     ##
##==========================================================================##

# CONTENTS
# 1. Fig. 3.19: mapping weight of European cities in ETMUN network 
# 2. Fig. 3.20: ANOVA nb of members etmun/administration level
# régression linéaire (pop/nb d'adhésion) - non significatif 
# Anova sur nb d'adhésion/population - non significatif 
# Anova sur nb d'adhésion/type de région (Nuts CP 14-20) - non significatif 


# Working directory huma-num
# setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

# Library
library(tidylog)
library(skimr)
library(tidyverse)
library(sf)
library(mapview)
library(cartography)
library(stringr)
library(readr)
library(ggrepel)




# ==== 1. Fig. 3.19: mapping weight of European cities in ETMUN network ==== 

## ----~~ Load data ----

etmun <- readRDS("../CITY/CorrectedDB/ETMUN_Membership_GNidCorr.RDS")
skim(etmun)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")
sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

## ----~~ Prepare data: cities in Europe with nb of members etmun ----
## rm na : removed 9 out of 17333 rows (<1%)
sfCitiesEur <- etmun %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

### transform to sf
sfCitiesEur <- st_as_sf(sfCitiesEur, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

### filter cities in Europe
# sfCitiesEur <- sfCitiesEur %>%  filter(Continent == "Europe")
sfCitiesEur <- st_intersection(sfCitiesEur, rec)

#mapview(sfCitiesEur)

## summarise members ETMUN by cities
sfCitiesEur <- sfCitiesEur %>% 
  group_by(geonameId, asciiName) %>% 
  summarise(nbMembers = n()) %>% 
  ungroup()


## ----~~ View distribution ----
freq <- as.data.frame(table(sfCitiesEur$nbMembers))

## create barplots
distrib <- ggplot(data = sfCitiesEur, aes(x = nbMembers)) +
  geom_histogram() +
  scale_y_continuous(trans = "log10")

## display end save
# pdf(file = "OUT/distrib.pdf", width = 8.3, height = 5.8)
# distrib
# dev.off()


## ----~~ Mapping with ggplot ----

### Map with squared values (to emphase the differences of proportionnality)
sfCitiesEur <- sfCitiesEur %>%
  mutate(nbMembers2 = nbMembers*nbMembers)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))


citiesEtmun2 <- ggplot() + 
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = sfCitiesEur %>% filter(nbMembers > 3) %>% st_centroid(),
          mapping = aes(size = nbMembers2), colour = "#ff620080", show.legend = NA) +
  geom_sf(data = sfCitiesEur %>% filter(nbMembers > 3) %>% st_centroid(),
          mapping = aes(size = nbMembers2), shape = 1, colour = "#ff6200", show.legend = NA) +
  scale_size(name = "Nombre d'adhésions par ville\naux associations transnationales\nde municipalités (ETMUN)",
             breaks = c(1849, 400, 200, 16),
             labels = c("43", "20", "10", "4"),
             range = c(0.5, 13)) +
  annotate("text", label = "Nom des villes : top 10\n\nLes villes comptant moins de 4 adhésions\nne figurent pas sur la carte", 
           size = 2.7, x = 1000000, y = 2600000, hjust = 0) +
  annotate("text", size = 3,
           label = str_c(ceiling(sum(sfCitiesEur$nbMembers)/100)*100, " adhésions\n", 
                   length(unique(etmun$Network_Name)), " associations ETMUN"),
           x = c(st_bbox(rec)[3]-1000000), y = c(st_bbox(rec)[4]-800000)) +
  annotate("text", label = "Source : ETMUN 2019 / PG, AD, 2019",
           size = 2.2, 
           hjust = 1,
           x = c(st_bbox(rec)[3]), y = c(st_bbox(rec)[2]-130000)) +
  labs(x = "", y = "") +
  geom_sf_text(data = sfCitiesEur %>% top_n(n = 10),
               aes(label = asciiName), size = 2.2, color = "#4d4d4d",
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
pdf(file = "OUT/propCitiesEtmun2.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
citiesEtmun2
dev.off()





# ==== 2. Fig. 3.20: ANOVA nb of members etmun/administration level ==== 


## ----~~ Load data ----
etmun <- readRDS("../CITY/CorrectedDB/ETMUN_Membership_GNidCorr.RDS")
uniqueGN <- readRDS("../CITY/Data/UniqueGN_info_AllDB_Corr.RDS")
admin <- read_delim("Script Analyse/admintyposimplifiee.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)
rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")


## ----~~ Prepare data: cities in Europe with nb of members etmun ----
## rm na : removed 9 out of 17333 rows (<1%)
sfCitiesEur <- etmun %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

### transform to sf
sfCitiesEur <- st_as_sf(sfCitiesEur, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

### filter cities in Europe
#sfCitiesEur <- sfCitiesEur %>%  filter(Continent == "Europe")
sfCitiesEur <- st_intersection(sfCitiesEur, rec)

#mapview(sfCitiesEur)

### summarise members ETMUN by cities
sfCitiesEur <- sfCitiesEur %>% 
  group_by(geonameId, asciiName) %>% 
  summarise(nbMembers = n()) %>% 
  ungroup()


## recode 'fcodeName'
uniqueGN <- left_join(uniqueGN, admin, by = "fcodeName")

### Add admin level to the data
sfCitiesEur <- left_join(sfCitiesEur, select(uniqueGN, geonameId, adminLevel), by = "geonameId")
citiesEur <- sfCitiesEur %>% 
  filter(!is.na(adminLevel)) %>% 
  st_drop_geometry()

### corrections before anova
citiesEur <- citiesEur %>% 
  filter(adminLevel != "other") %>% 
  mutate(adminLevel = recode(adminLevel, 
                             "seat of government of a political entity" = "capital of a political entity")) %>% 
  as.data.frame() 


skim(citiesEur)

### clean
rm(admin, uniqueGN)


### Save for EploratR
#write.csv2(citiesEur, "Script Analyse/citiesEur.csv", row.names = F, fileEncoding = "UTF-8")


## ----~~ ANOVA functions ----
## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

### Anova parameters (1 factor) --
AnovaTab <- function(df, varx, vary){
  groupMean <- round(tapply(df[, vary], df[, varx], mean, na.rm = TRUE), digits = 2)
  groupMedian <- round(tapply(df[, vary], df[, varx], median, na.rm = TRUE), digits = 2)
  groupVar <- round(tapply(df[, vary], df[, varx], var, na.rm = TRUE), digits = 2)
  tabGroup <- data.frame(Modalité = names(groupMean), 
                         Moyenne = groupMean,
                         Médiane = groupMedian,
                         Variance = groupVar, 
                         stringsAsFactors = FALSE)
  tabAll <- data.frame(Modalité = "Ensemble", 
                       Moyenne = round(mean(df[, vary]), digits = 2), 
                       Médiane = round(median(df[, vary]), digits = 2), 
                       Variance = round(var(df[, vary]), digits = 2), 
                       stringsAsFactors = FALSE)
  
  tabVariance <- rbind(tabGroup, tabAll)
  
  return(tabVariance)
}

### Anova plot (1 factor) -- labels not generalized
AnovaPlot <- function(df, varx, vary, tx, ty, source){
  
  xLevels <- sort(unique(df[, varx]))
  df$ID <- df[, varx]
  df$VAR <- df[, vary]
  
  if(length(xLevels) == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (length(xLevels) > 2){
    colPal <- brewer.pal(n = length(xLevels), name = "Set1")
  }
  
  # jitter points
  set.seed(99)
  df$JIT <- as.numeric(as.factor(df[, varx])) + sample(x = seq(-0.3, 0.3, 0.01), size = nrow(df), replace = TRUE)
  
  # mean segments
  groupMean <- tapply(df[, vary], df[, varx], mean, na.rm = TRUE)
  avgSegment <- data_frame(ID = names(groupMean), 
                           XMIN = seq(1, length(groupMean), 1) - 0.4,  
                           XMAX = seq(1, length(groupMean), 1) + 0.4, 
                           YMIN = groupMean, 
                           YMAX = groupMean)

  # residuals segments
  df <- df %>% left_join(x = ., y = avgSegment[, c(1, 4)], by = "ID")

  aovPlot <- ggplot() +
    geom_hline(yintercept = mean(df$VAR, na.rm = TRUE), color = "grey60", size = 1, linetype = 2) +
    geom_segment(data = avgSegment, aes(x = XMIN, xend = XMAX, y = YMIN, yend = YMAX), color = "grey40", size = 2) +
    geom_segment(data = df, aes(x = JIT, xend = JIT, y = YMIN, yend = VAR), color = "grey40", alpha = 0.5) +
    geom_point(data = df, aes(JIT, VAR, color = ID), show.legend = FALSE) +
    geom_label_repel(data = df %>% filter(VAR > 25 | asciiName == "Rotterdam"), 
                     aes(JIT, VAR, label = asciiName),
                     na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) +
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = tx, breaks = seq(1, length(groupMean), 1), labels = xLevels) +
    scale_y_continuous(name = ty) +
    labs(x = tx, y = ty) +
    theme_bw() +
    labs(caption = source) +
    theme(axis.text.x = element_text(size = 9, angle = 20, vjust = 0.6),
          plot.caption = element_text(size = 6))
  
  return(aovPlot)
}

### Compute linear model -- 
ComputeRegression <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df)
    linModSumry <- summary(linMod)
  } else {
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df)
    linModSumry <- summary(linMod)
  }
  coefReg <- round(linModSumry$coefficients, digits = 4)[, 1:2]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  adjR2 <- round(linModSumry$adj.r.squared, digits = 2)
  
  tabResid <- data.frame(ABSRESID = round(linModSumry$residuals, digits = 3), 
                         RELRESID = round(linModSumry$residuals / (df[, vardep] - linModSumry$residuals), digits = 3))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination ajusté",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  
  return(list(TABCOEF = tabResults, TABRESID = tabResid, COEF = coefReg))
}


## ----~~ Apply function and plot ----

### Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = citiesEur, varx = c("adminLevel"), vary = c("nbMembers")) 
resume <- ComputeRegression(citiesEur, vardep = "nbMembers", varindep = "adminLevel")
resume$TABCOEF
## R2 = 30%

### save tab
require(gridExtra)
require(grid)
pdf(file = "OUT/ANOVAtab_adh_admin_etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
grid.table(anovaTab, rows = NULL)
dev.off()


### Anova plot
require(RColorBrewer)
anovaPlot <- AnovaPlot(df = citiesEur, c("adminLevel"), c("nbMembers"), 
                       tx = "",
                       ty = "Nombre d'adhésions au réseau ETMUN",
                       source = "Source : ETMUN, 2019 / PG, AD, 2019")

### save plot
pdf(file = "OUT/ANOVAboxplot_adh_admin__etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
anovaPlot
dev.off()


# ==== annova plot (hors fonction) ==== 
xLevels <- sort(unique(citiesEur[, c("adminLevel")]))
citiesEur$ID <- citiesEur[, c("adminLevel")]
citiesEur$VAR <- citiesEur[, c("nbMembers")]

if(length(xLevels) == 2){
  colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
} else if (length(xLevels) > 2){
  colPal <- brewer.pal(n = length(xLevels), name = "Set1")
}

# jitter points
set.seed(99)
citiesEur$JIT <- as.numeric(as.factor(citiesEur[, c("adminLevel")])) + sample(x = seq(-0.3, 0.3, 0.01), size = nrow(citiesEur), replace = TRUE)

# mean segments
groupMean <- tapply(citiesEur[, c("nbMembers")], citiesEur[, c("adminLevel")], mean, na.rm = TRUE)
groupSd <- tapply(citiesEur[, c("nbMembers")], citiesEur[, c("adminLevel")], sd, na.rm = TRUE)

avgSegment <- data_frame(ID = names(groupMean), 
                         XMIN = seq(1, length(groupMean), 1) - 0.4,  
                         XMAX = seq(1, length(groupMean), 1) + 0.4, 
                         YMIN = groupMean, 
                         YMAX = groupMean)
bibi <- data_frame(ID = names(groupSd),
                   mean = groupMean,
                   sd = groupSd)

# residuals segments
citiesEur <- citiesEur %>% left_join(x = ., y = avgSegment[, c(1, 4)], by = "ID")
citiesEur <- citiesEur %>% left_join(x = ., y = bibi, by = "ID")

aovPlot <- ggplot() +
  geom_hline(yintercept = mean(citiesEur$VAR, na.rm = TRUE), color = "grey60", size = 1, linetype = 2) +
  geom_segment(data = avgSegment, aes(x = XMIN, xend = XMAX, y = YMIN, yend = YMAX), color = "grey40", size = 2) +
  geom_segment(data = citiesEur, aes(x = JIT, xend = JIT, y = YMIN, yend = VAR), color = "grey40", alpha = 0.5) +
  geom_point(data = citiesEur, aes(JIT, VAR, color = ID), show.legend = FALSE) +
  geom_label_repel(data = citiesEur %>% filter(VAR > 25 | asciiName == "Rotterdam"), 
                   aes(JIT, VAR, label = asciiName),
                   na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) +
  scale_color_manual(values = colPal) +
  scale_x_continuous(name = "tx", breaks = seq(1, length(groupMean), 1), labels = xLevels) +
  scale_y_continuous(name = "ty") +
  labs(x = "tx", y = "ty") +
  theme_bw() +
  labs(caption = "Source : ETMUN, 2019 / PG, AD, 2019") +
  theme(axis.text.x = element_text(size = 9, angle = 20, vjust = 0.6),
        plot.caption = element_text(size = 6))

pdf(file = "OUT/test.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
aovPlot
dev.off()



# ==== reprendre ici ==== 

#a nettoyer


######################################
# regression population/nb d'adhésion


## Load data
uniqueGN <- readRDS("Data/UniqueGNforETMUN.RDS")

## Add population to the data
sfCitiesEur <- left_join(sfCitiesEur, select(uniqueGN, geonameId, population), by = "geonameId")

sfCitiesEur$population <- as.numeric(sfCitiesEur$population)

skim(sfCitiesEur$population)

sfCitiesEur <- sfCitiesEur %>% 
  filter(population > 500)

# Filter value
thrshld <- 1

# Test R2 with threshold value
R2 <- list()

for(i in seq(1,5, 1)){
  reg <- lm(log10(nbMembers) ~ log10(population), 
            data = sfCitiesEur %>% dplyr::filter(nbMembers > i))
  R2[[i]] <- summary(reg)$r.squared
}


##

## display graph
regCities <- ggplot(sfCitiesEur %>% dplyr::filter(nbMembers > thrshld), 
                    aes(x = population, y = nbMembers)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "pop", 
       y = "Nombre d'adhésions à des associations de municipalités") 
regCities

### Estimer la regression linéaire
require(stats)
reg <- lm(log10(nbMembers) ~ log10(population), 
          data = sfCitiesEur %>% dplyr::filter(nbMembers > thrshld))
summary(reg)

### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regCities + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 1000, y = 20, label = paste0(eq, "\nR2 = ", 
                                                              round(summary(reg)$r.squared, 2))) 





## anova type de régions
NUTS_CP_1420 <- st_read("../KEEP/AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
  st_transform(3035)

citiesEurNCP <- st_intersection(sfCitiesEur, select(NUTS_CP_1420, TYPE))
citiesEurNCP <- citiesEurNCP %>% as.data.frame() %>% select(-geometry) 

### Save for EploratR
write.csv2(citiesEurNCP, "Script Analyse/citiesEurNCP.csv", row.names = F, fileEncoding = "UTF-8")
      
## Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = citiesEurNCP, varx = 6, vary = 3) # variables position in the df, not name
anovaTab

anovaPlot <- AnovaPlot(df = citiesEurNCP, varx = 6, vary = 3, 
                       tx = "type region",
                       ty = "Nombre d'adhésion") # titre des axes inactif

anovaPlot

resume <- ComputeRegression(citiesEurNCP, vardep = "nbMembers", varindep = "TYPE")


NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
  st_transform(3035)



## anova type de région (Nord, Sud, est, Ouest)
### Add 'Region' variable
sfCitiesEur <- left_join(sfCitiesEur, select(etmun, geonameId, Region), by = "geonameId")
mapview(sfCitiesEur)
citiesEur <- sfCitiesEur %>% 
  filter(!duplicated(geonameId)) %>% 
  filter(!is.na(Region)) %>% 
  as.data.frame() %>% 
  select(-geometry)

freq <- as.data.frame(table(citiesEur$Region))

citiesEur <- citiesEur %>% 
  filter(Region %in% c("Northern Europe", "Southern Europe", 
                       "Eastern Europe", "Western Europe"))

### Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = citiesEur, varx = 5, vary = 3) # variables position in the df, not name

resume <- ComputeRegression(citiesEur, vardep = "nbMembers", varindep = "Region")

anovaPlot <- AnovaPlot(df = citiesEur, varx = 5, vary = 3, 
                       tx = "type region",
                       ty = "Nombre d'adhésions") # titre des axes inactif


