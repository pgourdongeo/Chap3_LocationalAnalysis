###############################################################################
#               ANALYSE BIVARIEE : participations ~ taille des agglo
#
# DESCRIPTION : régression linéaire et cartographie des résidus
# 
# PG, AD, Octobre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")
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
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfEU <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035) %>% 
  st_make_valid()

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)



# UMZ

## Count participations in umz
### Intersect umz and participations
inter <- st_intersects(umz, sfPartner)
### Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

# 2284 / 3962 umz with no project :
# sum(umz$n == 0)
# 3507 umz with less than 11 projects
# sum(umz$n < 11)

## display graph
### with log10
regUmz <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population 2011 des agglomérations UMZ (log10)", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ (log10)") 
regUmz

### Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > 10))

### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regUmz + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 


## Residuals
### add residuals and standart residuals to df
umz <- umz %>% 
  filter(n > 10)%>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(umz$rezStand)

## Outliers
### Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2 * sdRez | x > 2 * sdRez)
  
}

### Ajout d'une variable Outlier au DF
umz <- umz %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## Plot with outliers and save pdf
#pdf(file = "AD/OUT/lm_umz.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regUmz + 
  geom_label_repel(data = umz %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(Name, Country, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black") + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 
dev.off()



## residuals map (residuals standart)
### FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, var, titleLeg){
  
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
              sources = "sources :", 
              author = "PG, AD, 2019", 
              horiz = F,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}

### defines a set of breaks and colors 
bks <- c(min(umz$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(umz$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

### Plot and save
#pdf(file = "AD/OUT/rez_umz.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap(frame = rec, 
       bgmap = sfEU, 
       units = umz %>% filter(n > 10), 
       var = "rezStand",
       titleLeg = "résidus standardisés\n")
dev.off()


# FUA

