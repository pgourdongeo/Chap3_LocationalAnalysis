###############################################################################
#                               explo nuts
#
# DESCRIPTION : 
# 
# PG, AD, Novembre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")
setwd("~/git/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)


# library
library(tidyverse)
library(sf)
library(skimr)
library(lwgeom)
library(mapview)


# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

NUTS_CP_1420 <- st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp") %>% 
  st_transform(3035)

NUTS_EF_0613 <-st_read("AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp") %>%
  st_transform(3035)



# FUNCTIONS

## ANOVA
## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

### Function Anova parameters (1 factor) ---- 
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

### Function Anova plot (1 factor) ---- modifié
AnovaPlot <- function(df, varx, vary, tx, ty){
  
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
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = tx, breaks = seq(1, length(groupMean), 1), labels = xLevels) +
    scale_y_continuous(name = vary) +
    labs(x = tx, y = ty) +
    theme_bw()
  
  return(aovPlot)
}

## Count participations in Nuts
countP <- function(df1, df2){
  
  ### Intersect umz and participations
  inter <- st_intersects(df1, df2)
  ### Count points in polygons
  df1 <- st_sf(df1, 
               n = sapply(X = inter, FUN = length), 
               geometry = st_geometry(df1))
  return(df1)
}




# ANOVA Nuts CP

## Visu control
mapview(sfEU) + mapview(NUTS_CP_1420) + mapview(sfPartner)
unique(NUTS_CP_1420$TYPE)

## Count participations in each nuts
nutsCP <- countP(NUTS_CP_1420, sfPartner)
nutsCP <- nutsCP %>% as.data.frame() %>% select(-geometry) 
sum(nutsCP$n)

### Save for EploratR
write.csv2(nutsCP, "nutsCP.csv", row.names = F, fileEncoding = "UTF-8")

### Check points not joined
#### join nuts type to participation points
sfPart <- st_intersection(sfPartner, select(NUTS_CP_1420, NUTS_ID, TYPE))
part <- sfPart %>% as.data.frame() %>% select(-geometry)
partOut <- anti_join(sfPartner, part)
mapview(NUTS_CP_1420) + mapview(partOut) # some are in water (generalization)

## nuts without type
nutsCP$TYPE <- ifelse(is.null(nutsCP$TYPE), nutsCP$TYPE == "Hors typologie", nutsCP$TYPE)
nutsCP <- nutsCP %>% 
  mutate(HTYPO = is.null(TYPE),
         TYPO = ifelse(HTYPO == TRUE, TYPO == "Hors typologie", TYPE))
## Stat summary
anovaTab <- AnovaTab(df = nutsCP, varx = 5, vary = 6) # variables position in the df, not name

## Anova plot
anovaPlot <- AnovaPlot(df = nutsCP, varx = 5, vary = 6, 
                       tx = "Type de Nuts",
                       ty = "Nombre de participations") # titre des axes inactif
                       
anovaPlot








# Nuts EF
mapview(sfEU) + mapview(NUTS_EF_0613) + mapview(sfPartner)

unique(NUTS_EF_0613$TYPE)






