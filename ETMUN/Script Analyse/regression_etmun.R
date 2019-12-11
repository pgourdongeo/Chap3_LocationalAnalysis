###############################################################################
#               ANALYSE BIVARIEE : adhesion ~ taille des agglo
#
# DESCRIPTION : régression linéaire et cartographie des résidus
# 
# PG, AD, Octobre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

# Library
library(cartography)
library(dplyr)
library(sf)
library(tidylog)
library(skimr)
library(lwgeom)
library(ggplot2)
library(ggrepel)
#library(ggpubr)
#library(GGally)


# Import data


EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F) 
EtmunPoints <- EtmunPoints %>% filter(!is.na(lon))

sfAdhesion <- st_as_sf(EtmunPoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)



sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")




umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)

fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp") %>% 
  st_transform(crs = 3035)



# FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, var, source, titleLeg){
  
  par(mar = c(0, 0, 0, 0)) 
  bb <- st_bbox(frame)
  
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#f0f0e9", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = var, border = NA, breaks= bks, col= cols, 
             legend.pos = var, add = TRUE)
  
  # Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8,
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = F, 
              values.rnd = 2, 
              col = cols)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = source, 
              author = "PG, AD, 2019", 
              horiz = F,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}


rezMap_propChoro <- function(frame = rec, bgmap = sfEU, units, var, myVal, var2, 
                             title1, title2, labels, source) {
  
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5)
  propSymbolsChoroLayer(units, 
                        var = var, inches = 0.3, border = "grey60", lwd = 0.5, symbols = "square",
                        var2 = var2, breaks = bks, col = cols,
                        legend.var.pos = NA, legend.var2.pos = NA)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, add = TRUE)
  
  # Add legend
  legendSquaresSymbols(pos = c(1000000, 4200000),
                       cex = 1,
                       var = myVal,
                       inches = 0.3, border = "grey60", lwd = 0.5, col = NA,
                       title.txt = title1, title.cex = 0.8, values.cex = 0.6)
  
  legendChoro(pos = c(1000000, 3000000), 
              cex = 0.9,
              title.txt = title2, title.cex = 0.8, values.cex = 0.7,
              breaks = bks, col = cols, values.rnd = 2, nodata = F)
  
  
  # Add an explanation text
  text(x = 1000000, y = 2600000, labels = labels, cex = 0.7, adj = 0)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = source, 
              author = "PG, AD, 2019", 
              horiz = FALSE,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}

# Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2.5 * sdRez | x > 2.5 * sdRez)
  
}




#========= UMZ ==========

## Count participations in umz
### Intersect umz and participations
inter <- st_intersects(umz, sfAdhesion)
### Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

summary(umz$n)
# 2011 / 3962 umz with no project :
# sum(umz$n == 0)
# 3869 umz with less than 11 projects
# sum(umz$n < 11)


# Filter value
thrshld <- 2
# Test R2 with threshold value
R2 <- list()

for(i in seq(1,20, 1)){
  reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > i))
  R2[[i]] <- summary(reg)$r.squared
}


## display graph
### with log10


regUmz <- ggplot(umz %>% dplyr::filter(n > thrshld), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population 2011 des agglomérations UMZ (log10)", 
       y = "Nombre d'adhésions à des associations de municipalités (log10)") 
regUmz

### Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > thrshld))
summary(reg)

### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regUmz + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 25000, y = 1000, label = paste0(eq, "\nR2 = ", 
                                                              round(summary(reg)$r.squared, 2))) 


## Residuals
### add residuals and standart residuals to df
umz <- umz %>% 
  filter(n > thrshld)%>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(umz$rezStand)

## Outliers
### Ajout d'une variable Outlier au DF
umz <- umz %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## Plot with outliers and save pdf
pdf(file = "OUT/lm_umz_etmun.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regUmz + 
  geom_label_repel(data = umz %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(Name, Country, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "label", x = 3000000, y = 10, label= paste0(eq, "\nR2 = ", 
           round(summary(reg)$r.squared, 2)), hjust = 0, fill = "#E69F00", size = 3) +
  labs(caption = "Sources : ETMUN 2019 ; Tradeve 2015", size = 3) 
dev.off()


## residuals map (residuals standart)
### defines a set of breaks and colors 
bks <- c(min(umz$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(umz$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

skim(umz$Pop2011)

### Plot and save
pdf(file = "OUT/rez_umz_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = umz %>% filter(n > thrshld),
                 var = "Pop2011", 
                 myVal = c(10000, 1000000, 5000000, 10000000),
                 var2 = "rezStand",
                 title1 = "Population des UMZ en 2011", 
                 title2 = "Résidus Standardisés*",
                 labels = paste("*Résidus de la régression :\n", eq,",\ndiscrétisés selon la moyenne\ndes résidus (=0) et 1 écart-type", sep = ""),
                 source = "Sources : ETMUN 2019 ; Tradeve 2015")
dev.off()




#========= FUA ==========

## select only LUZ 
fua <- fua %>% filter(URAU_CATG == "L")

## Count participations in FUA
### Intersect fua and participations
inter <- st_intersects(fua, sfAdhesion)
### Count points in polygons
fua <- st_sf(fua, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(fua))

## display graph
### with log10
regFua <- ggplot(fua , aes(x = URAU_POPL, y = n)) +
  geom_point () +
  theme_light() +
  labs(x = "Population des aires urbaines fonctionnelles (FUA) en 2006\n(log10)", 
       y = "Nombre d'adhésions à des associations de municipalités (log10)") 
regFua

regFua <- ggplot(fua %>% dplyr::filter(n > 1), aes(x = URAU_POPL, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population des aires urbaines fonctionnelles (FUA) en 2006\n(log10)", 
       y = "Nombre d'adhésions à des associations de municipalités (log10)") 
regFua


### Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(URAU_POPL) , data = fua %>% dplyr::filter(n > 1))
summary(reg)
### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regFua + 
  geom_abline(intercept = reg$coefficients[1], reg$coefficients[2], color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 100000, y= 250, label= paste0(eq, "\nR2 =" ,  round(summary(reg)$r.squared, 3)),
           color="black") 

## Residuals
### add residuals and standart residuals to df
fua <- fua %>% 
  filter(n > 10) %>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(fua$rezStand)

## Outliers
### Ajout d'une variable Outlier au DF
fua <- fua %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## Plot with outliers and save pdf
pdf(file = "AD/OUT/lm_fua.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regFua + 
  geom_label_repel(data = fua %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(URAU_NAME, CNTR_CODE, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black") + 
  geom_abline(intercept = -0.54, slope = 0.38, color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 100000, y= 300, label= paste0(eq, "\nR2 = 0.25"),
           color="black") 
dev.off()



## residuals map (residuals standart)
### defines a set of breaks and colors 
bks <- c(min(fua$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(fua$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

### Plot and save
#pdf(file = "AD/OUT/rez_fua.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap(frame = rec, 
       bgmap = sfEU, 
       units = fua %>% filter(n > 10), 
       var = "rezStand",
       source = "Sources :",
       titleLeg = "résidus standardisés\n")
dev.off()

