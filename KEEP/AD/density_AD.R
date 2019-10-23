###############################################################################
#                     EXPLORATION DE LA BD 'Project Partner' 
#
# DESCRIPTION : analyse des densités des projets selon différents maillages 
# (nuts, umz, fua, carroyage) et pas de temps
# PG, AD
# Octobre 2019
##############################################################################


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

# Visu data 
par(mar = c(0,0,0,0))
plot(st_geometry(sfEU))
plot(st_geometry(sfPartner), pch = 0, cex = 0.5, col = "blue", add = T)
plot(st_geometry(rec), add = T)



###############################################################
# Cartography particiation/tessel
###############################################################

# Functions

## Build a regular grid and count points in each cell
pt_in_grid <- function(feat, adm, cellsize){
  
  ptgrid <- list()
  
  # Create a regular grid (adm bbox)
  grid <- st_make_grid(x = adm, cellsize = cellsize, what = "polygons")
  
  # Keep only cells that intersect adm
  . <- st_intersects(grid, adm)
  grid <- grid[sapply(X = ., FUN = length)>0]
  
  # Count pts in grid
  . <- st_intersects(grid, feat)
  grid <- st_sf(n = sapply(X = ., FUN = length), grid)
  
  # cut cells in the borders
  ptgrid[["grid"]] <- st_intersection(grid, adm)
  
  # remove null values
  ptgrid[["grid0"]] <- grid %>% 
    filter(n != 0)
  
  return(ptgrid)
}

## Plot a map
plot_grid <- function(grid, adm, sources, bks, col, titleLeg){
  
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  # plot
  plot(st_geometry(rec), border = NA, col = "#A6CAE0")
  plot(st_geometry(adm), col = "ivory1", add = T)
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.5, add = T)
  plot(st_geometry(rec), border = "black", col = NA, add = T)
  
  # Add a layout
  layoutLayer(title = "",
              sources = sources, 
              author = "PG,AD, 2019", 
              horiz = F,
              col = NA,
              frame = F,
              scale = 100, 
              posscale = "bottomleft")
  
  ## Add legend
  legendChoro(pos = "topleft", 
              title.cex = 0.8, 
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
}


# Map

## 50 km cells
europegrided <- pt_in_grid(feat = sfPartner, adm = sfEU, cellsize = 50000)
#skim(europegrided[[1]])
#skim(europegrided[[2]])

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Plot
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          sources = "Sources : ", 
          bks = bks, 
          col = cols, 
          titleLeg = "Nombre de participations\npar carreau de 2 500 km2")





################################################
# Cartography participation/inhabitant(nuts2)
################################################

# import data nuts2/3 
nuts23 <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()
  
plot(st_geometry(nuts23))


# Intersect nuts and participations
inter <- st_intersects(nuts23, sfPartner)

# Count points in polygons
nuts23 <- st_sf(nuts23, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(nuts23))

## Display prop map
# par(mar = c(1, 4, 1, 4))
# plot(st_geometry(nuts23), col = "ivory1", border = "ivory3",lwd =0.5,bg = "#FBEDDA")
# propSymbolsLayer(nuts23, var = "n", inches = 0.1)


# Density
nuts23 <- nuts23 %>% 
  mutate(density = n / Pop_t_2001 * 10000)

## skim
skim(nuts23)

## Geometric intervals
nuts23_0 <- nuts23 %>% 
  filter(density != 0)
hist(nuts23_0$density, probability = TRUE, breaks = bks, col = "#F0D9F9")
rug(nuts23_0$density)

# defines a set of breaks and colors
bks <- c(0, getBreaks(v = nuts23_2$density, method = "geom", nclass = 6))
#bks <- getBreaks(v = nuts23_2$density, method = "geom", nclass = 6)
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

# FUNCTION - Display the map
densMap <- function(frame, bgmap, units, titleLeg){
  
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0")
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = "density", border = NA, breaks= bks, col= cols, 
             legend.pos = "density", add = TRUE)
  plot(st_geometry(units), col = NA, border = "ivory4", lwd = 0.3, add = T)
  plot(st_geometry(frame), border = "black", col = NA, add = T)
  
  # Add legend
  legendChoro(pos = "topleft", 
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
              scale = 100, 
              posscale = "bottomleft")
  
}

# Plot the density map
densMap(frame = rec, bgmap = sfEU, units = nuts23, 
        titleLeg = "Nombre de participations\npour 10 000 habitants")




######################################
# Barplots participation/type of nuts
######################################

# participation rate in nuts
nuts23 <- nuts23 %>% 
  mutate(p = n * 100 / (sum(nuts23$n)))

# display barplots
p <- ggplot(data = nuts23, aes(x = Typo7, y = p)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_light()
  
p



######################################
# Visualisation participation/umz
######################################

# import data 
umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)

# Intersect umz and participations
inter <- st_intersects(umz, sfPartner)

# Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

umz <- umz %>% 
  mutate(rank11 = row_number(desc(Pop2011)))

plot(st_geometry(umz))
# display barplot
n <- ggplot(umz %>% dplyr::filter(n>10), aes(x = rank11, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  # scale_x_continuous(trans = 'log10') +
  labs(x = "rang umz (pop 2011)", y = "Nombre de participations")

n

n2 <- ggplot(umz %>% dplyr::filter(n>10), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "umz pop 2011 (log10)", 
       y = "Nombre de participations (log10") +
  geom_smooth(method = 'lm')
n2

# ggscatter(umz, x = "Pop2011", y = "n", add = 'reg.line') +
#   stat_cor(label.y = 300) +
#   stat_regline_equation(label.y = 200)

# Estimer la regression linéaire
require(stats)
reg<-lm(log10(Pop2011) ~ log10(n), data = umz %>% dplyr::filter(n>10))
reg

coeff=coefficients(reg)
# Equation de la droite de regression :
eq = paste0("y = ", round(coeff[2],2), "*x + ", round(coeff[1],1))
# Graphe
n2 + geom_abline(intercept = 3.7, slope = 0.96, color="red",
                 linetype="dashed", size=1.5) +
  ggtitle(eq) + 
  annotate(geom="text", x=15000, y=250, label="R2 = 0.33",
                  color="blue")

summary(reg)

residuals(reg)

library(purrr)
rez <- reg %>% 
  map("residuals") 

umz <- umz %>% 
  mutate(Yest = (0.97 * log10(n)) + 3.7) %>% 
  mutate(rez = log10(Pop2011) - Yest)


# FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, units2, titleLeg){
  
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0")
  plot(st_geometry(bgmap), col = "white", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = "rez", border = NA, breaks= bks, col= cols, 
             legend.pos = "rez", add = TRUE)
  #plot(st_geometry(units), col = NA, border = "ivory4", lwd = 0.05, add = T)
  plot(st_geometry(frame), border = "black", col = NA, add = T)
  
  # Add legend
  legendChoro(pos = "topleft", 
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
              scale = 100, 
              posscale = "bottomleft")
  
}


# defines a set of breaks and colors
bks <- c(-1.26, -1, -0.5, 0.5, 1, 1.52)
cols <- c("#1A7832","#AFD4A0", "#e8deae", carto.pal("wine.pal",2))
# Plot the residuals map
rezMap(frame = rec, bgmap = sfEU, units = umz %>% filter(n>10), 
        titleLeg = "résidus\n(455/3962 umz avec n > 10)")


######################################
# Visualisation participation/fua
######################################

# import data 
fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp") %>% 
  st_transform(crs = 3035)

# Intersect fua and participations
inter <- st_intersects(fua, sfPartner)

# Count points in polygons
fua <- st_sf(fua, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(fua))

fua <- fua %>% 
  mutate(rankpop = row_number(desc(URAU_POPL)))


# display barplot
n <- ggplot(fua %>% dplyr::filter(n>10), aes(x = rankpop, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  # scale_x_continuous(trans = 'log10') +
  labs(x = "rang fua (pop 2006)", y = "Nombre de participations")

n

n2 <- ggplot(fua %>% dplyr::filter(n>10), aes(x = URAU_POPL, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population des aires urbaines fonctionnelles en 2006\n(log10)", 
       y = "Nombre de participations\n(log10)") +
  geom_smooth(method = 'lm')
n2


# Estimer la regression linéaire
require(stats)
reg<-lm(log10(URAU_POPL) ~ log10(n), data = fua %>% dplyr::filter(n>10))
reg
summary(reg)
coeff=coefficients(reg)
# Equation de la droite de regression :
eq = paste0("y = ", round(coeff[2],2), " * x + ", round(coeff[1],1))
# Graphe
n2 + geom_abline(intercept = 4.45, slope = 0.66, color="red",
                 linetype="dashed", size=1.5) +
  #ggtitle(eq) + 
  annotate(geom="text", x=100000, y=250, label = paste0(eq, "\nR2 = 0.25"),
           color="blue")



