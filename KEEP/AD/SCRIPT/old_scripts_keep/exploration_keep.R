
##==========================================================================##         
##            EXPLORATION DE LA BD EUCICOP/KEEP                             ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : analyse des densités des projets selon différents          ##
##               maillages (nuts, umz, fua, carroyage) et pas de temps      ##
##                                                                          ##
## PG, AD, Octobre 2019                                                     ##
##==========================================================================##


# Working directory huma-num
# setwd("~/BD_Keep_Interreg/KEEP")

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
library(readr)
library(RColorBrewer)
library(mapview)
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
plot(st_geometry(sfPartner), pch = 1, cex = 0.1, col = "blue", add = T)
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(rec), add = T)
mapview(sfPartner)
mapview(sfEU)

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
plot_grid <- function(grid, adm, rec, sources, bks, col, titleLeg){
  
  bb <- st_bbox(rec)
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  plot(st_geometry(rec), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  #plot(st_geometry(adm), col = "ivory1")
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.5, add = T)
  #plot(st_geometry(rec), border = "black", col = NA, add = T)
  
  ## Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8, 
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
  
  # Add a layout
  layoutLayer(#extent = rec,
              title = "",
              sources = sources,
              author = "PG,AD, 2019",
              horiz = F,
              col = NA,
              frame = F,
              scale = 500,
              posscale = "bottomleft"
              )
  
}

## Plot 3 maps with a unique legend
plot_grids <- function(grid1, grid2, grid3, 
                       title1, title2, title3,
                       adm, rec, sources, bks, col, titleLeg){
  
  bb <- st_bbox(rec)
  
  ## plot 3 maps on a single figure 
  par(mar = c(0, 0, 0, 0), mfrow = c(2, 2), ps=15)
  
  ## plot1
  plot(st_geometry(rec), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.2, add = T)
  
  # Add title
  mtext(text = title1,
        font = 2,
        side = 3, 
        line = -1, 
        adj = 0,
        cex =0.6)
  
  ## Add legend
  legendChoro(pos = c(1000000, 1500000),
              title.cex = 0.7,
              values.cex = 0.5,
              title.txt = titleLeg,
              breaks = bks,
              nodata = FALSE,
              values.rnd = 0,
              col = cols)
  
  
  ## plot2
  plot(st_geometry(rec), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid2, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.2, add = T)
  
  # Add title
  mtext(text = title2,
        font = 2,
        side = 3,
        line = -1,
        adj = 0,
        cex =0.6)

  
  ## plot3
  plot(st_geometry(rec), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid3, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.2, add = T)
  
  # Add title
  mtext(text = title3,
        font = 2,
        side = 3,
        line = -1,
        adj = 0,
        cex =0.6)
  

  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1, 
        adj = 0,
        cex =0.35)
  
}


# hors fonction
# Create a regular grid (adm bbox)
grid <- st_make_grid(x = sfEU, cellsize = 50000, what = "polygons")

# Keep only cells that intersect adm
. <- st_intersects(grid, sfEU)
grid <- grid[sapply(X = ., FUN = length)>0]

# Count pts in grid
. <- st_intersects(grid, sfPartner)
grid <- st_sf(n = sapply(X = ., FUN = length), grid)
sum(grid$n) # = 29833

# cut cells in the borders
grid <- st_intersection(grid, sfEU)
sum(grid$n)

# ##tableaux de contingence et jointure :
# TableComptageRepUMZ_TypoUrbRur <- as.data.frame(table(NUTS_UrbainRural$Typo7))
# NUTS_UrbainRural
# UMZCitiesPoints
# JointUMZ_TypoUrbRural <- st_join(UMZCitiesPoints,NUTS_UrbainRural, join =st_intersects, left = T)
# 
# foo <- as.data.frame(table(JointUMZ_TypoUrbRural$Typo7))
# 
# JointSmallUMZ_TypoUrbRur <- JointUMZ_TypoUrbRural %>% filter(X2011<50000)
# 
# foo2 <- as.data.frame(table(JointSmallUMZ_TypoUrbRur$Typo7))
# 
# 3962 - sum(foo$Freq)
# 
# 2862-sum(foo2$Freq)
# 
# TableComptageRepUMZ_TypoUrbRur <- cbind(TableComptageRepUMZ_TypoUrbRur,foo,foo2)
# 
# ## Gestion des outsiders
# 
# Outsiders <- sfPartner %>% filter(is.na(Typo7))
# mapview(NUTS_UrbainRural, zcol="Typo7") +mapview(Outsiders)
# TableComptageRepUMZ_TypoUrbRur <- cbind(TableComptageRepUMZ_TypoUrbRur,foo,foo2)
# 
# Outsiders <- Outsiders %>% select(-c(37:72))
# 
# dist10km <- function(x,y){
#   
#   output <-  map(st_is_within_distance( x, y ,dist = 10000),1)
#   
#   return(output)
#   
# }
# 
# JoinOutsiders_Nuts <- st_join(Outsiders,NUTS_UrbainRural, join = dist10km)
# mapview(NUTS_UrbainRural, zcol="Typo7") +mapview(JoinOutsiders_Nuts)

# Map
# Create a regular grid (adm bbox)
grid <- st_make_grid(x = sfEU, cellsize = 50000, what = "polygons")

# Keep only cells that intersect adm
. <- st_intersects(grid, sfEU)
grid <- grid[sapply(X = ., FUN = length)>0]

# Count pts in grid
. <- st_intersects(grid, sfPartner)
grid <- st_sf(n = sapply(X = ., FUN = length), grid)
sum(grid$n) # = 29833

# cut cells in the borders
grid <- st_intersection(grid, sfEU)
sum(grid$n)
#=============
# Create a regular grid (adm bbox)
grid <- st_make_grid(x = sfEU, cellsize = 50000, what = "polygons")

# Keep only cells that intersect adm
. <- st_intersects(grid, sfEU)
grid <- grid[sapply(X = ., FUN = length)>0]

# cut cells in the borders
grid <- st_intersection(grid, sfEU)

# Count pts in grid
. <- st_intersects(grid, sfPartner)
grid <- st_sf(n = sapply(X = ., FUN = length), grid)

sum(grid$n) # = 28904


## 50 km cells
europegrided <- pt_in_grid(feat = sfPartnerSpe, adm = sfEU, cellsize = 50000)
#skim(europegrided[[1]])
#skim(europegrided[[2]])

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("brown.pal", length(bks) - 2))

# display.brewer.all()
# cols <- c("#e5dddb", brewer.pal(n = length(bks) - 2, name = "Orange"))

# st_bbox(europegrided[[1]])
# st_bbox(sfEU)
# bb <- st_bbox(rec)

## Plot
#pdf(file = "europeGrid_eucicopall.pdf",width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          rec = rec,
          sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2018 ", 
          bks = bks, 
          col = cols, 
          titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2")
dev.off()

# Period Maps
## load data
Partner2 <- read_delim("DataSource/PartnersIDProj.csv",
                       ";", escape_double = FALSE, locale = locale(),
                       trim_ws = TRUE)

Projects <- read.table("DataSource/ProjectsID.csv", 
                       sep=";", dec=".", 
                       #quote="",
                       header = TRUE,
                       encoding="UTF-8")

## join period to participations
partPeriod <- left_join(x = select(Partner2, ID_PARTICIPATION, ID_PARTNER, ID_PROJECT, lon, lat),
                  y = select(Projects, Period, ID_PROJECT),
                  by = "ID_PROJECT")

## tibble to sf
sfPartPeriod <- st_as_sf(partPeriod, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

# Visu data 
par(mar = c(0,0,0,0))
plot(st_geometry(sfEU))
plot(st_geometry(sfPartPeriod), pch = 0, cex = 0.5, col = "blue", add = T)
plot(st_geometry(rec), add = T)



## defines a unique set of breaks for all maps 
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("brown.pal", length(bks) - 2))


## prepare grids
europegrided1 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2000-2006"), 
                           adm = sfEU, cellsize = 50000)
europegrided2 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2007-2013"), 
                            adm = sfEU, cellsize = 50000)
europegrided3 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2014-2020"), 
                            adm = sfEU, cellsize = 50000)

## to do : % empty
# 7266 carreaux
hist(europegrided1[[1]]$n)
dplyr::filter(europegrided1[[1]], n == 0)
skim(europegrided1[[1]])

## display maps and save pdf
pdf(file = "europeGridPeriod_eucicopall.pdf", width = 8.3, height = 5.8)
plot_grids(grid1 = europegrided1[[1]], 
           grid2 = europegrided2[[1]],
           grid3 = europegrided3[[1]],
           adm = sfEU,
           rec = rec,
           sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2018\nPG, AD, 2019", 
           bks = bks, 
           col = cols, 
           title1 = "2000-2006",
           title2 = "2007-2013",
           title3 = "2014-2020",
           titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2")
dev.off()




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

# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfEU <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035) %>% 
  st_make_valid()

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)
#plot(st_geometry(umz))


# Intersect umz and participations
inter <- st_intersects(umz, sfPartner)

# Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

# 2284 / 3962 umz with no project :
# sum(umz$n == 0)
# 3507 umz with less than 11 projects
# sum(umz$n < 11)

# # rank
# umz <- umz %>% 
#   mutate(rank11 = row_number(desc(Pop2011)))
# 
# # display barplot with rank
# n <- ggplot(umz %>% dplyr::filter(n>=10), aes(x = rank11, y = n)) +
#   geom_point () +
#   theme_light() +
#   scale_y_continuous(trans = 'log10') +
#   # scale_x_continuous(trans = 'log10') +
#   labs(x = "rang umz (pop 2011)", y = "Nombre de participations")
# 
# n
# display barplot with log10
regUmz <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  labs(x = "Population 2011 des agglomérations UMZ ", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ ") 
#+ geom_smooth(method = 'lm')
regUmz
# display barplot with log10
regUmz <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population 2011 des agglomérations UMZ (log10)", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ (log10)") 
#+ geom_smooth(method = 'lm')
regUmz

# ggscatter(umz, x = "Pop2011", y = "n", add = 'reg.line') +
#   stat_cor(label.y = 300) +
#   stat_regline_equation(label.y = 200)

# Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > 10))
coeff <- coefficients(reg)
reg$coefficients

summary(reg)

# Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

# Graphe
regUmz + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  #ggtitle(eq) + 
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 



summary(reg)
residuals(reg)

# library(purrr)
# rez <- reg %>% 
#   map("residuals") 

# add residuals and standart residuals
umz <- umz %>% 
  filter(n > 10)%>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(umz$rezStand)

# # Fonction pour identifier des outliers dans une distribution :
# is_outlier <- function(x) {
#   
#   return(x < quantile(x, 0.25) - (1.5 * IQR(x)) | x > quantile(x, 0.75) + 1.5 * IQR(x))
#   
# }
# 
# a <- umz %>% 
#   filter(n > 10) %>% 
#   st_drop_geometry() %>% 
#   select(rez)
# 
# 1.5*IQR(a$rez)
# quantile(a$rez, 0.25)-0.8361051
# quantile(a$rez, 0.75)+0.8361051
# 
# d <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = rez)) +
#   geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
#   geom_density(alpha = .2, fill = "#FF6666") +
#   geom_vline(aes(xintercept = -1.079464),
#                  color = "blue", linetype = "dashed", size=1) +
#   geom_vline(aes(xintercept = 1.150149),
#              color = "blue", linetype = "dashed", size=1)

# # Ajout d'une variable Outlier au DF
# umz2 <- umz %>% 
#   group_by(n10 = n > 10) %>%
#   mutate(outlier_rez = ifelse(is_outlier(rez), 
#                               rez, 
#                               as.numeric(NA)))

# Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2 * sdRez | x > 2 * sdRez)
  
}

# Ajout d'une variable Outlier au DF
umz <- umz %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

# add outliers name to the plot
pdf(file = "lm_umz.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regUmz + 
  geom_label_repel(data = umz %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(Name, Country, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black") + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 
dev.off()


# FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, units2, var, titleLeg){
  
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  bb <- st_bbox(frame)
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#f0f0e9", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = var, border = NA, breaks= bks, col= cols, 
             legend.pos = var, add = TRUE)
  #plot(st_geometry(units), col = NA, border = "ivory4", lwd = 0.05, add = T)
  
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
              posscale = "bottomleft")
  
}

# # residuals 
# ## defines a set of breaks and colors 
# bks <- c(-1.26, -1, -0.5, 0.5, 1, 1.52)
# cols <- c("#1A7832","#AFD4A0", "#e8deae", carto.pal("wine.pal",2))
# 
# ## Plot the residuals map
# rezMap(frame = rec, 
#        bgmap = sfEU, 
#        units = umz %>% filter(n > 10), 
#        var = "rez",
#        titleLeg = "résidus\n(umz avec moins de 11 projets = 88%)")

# residuals standart
## defines a set of breaks and colors 
#bks <- c(-1, -0.5, 0, 0.5, 1)
#cols <- carto.pal("green.pal",2 ,"wine.pal",2)
bks <- c(min(umz$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(umz$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

## Plot the residuals map
#pdf(file = "rezS.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap(frame = rec, 
       bgmap = sfEU, 
       units = umz %>% filter(n > 10), 
       var = "rezStand",
       titleLeg = "résidus standardisés\n")
dev.off()





######################################
# Visualisation participation/fua
######################################

# import data 
fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp") %>% 
  st_transform(crs = 3035)

plot(st_geometry(fua))

idf <- fua %>% 
  filter(URAU_NAME == "Paris")
plot(st_geometry(idf))

## filter 
fua <- fua %>% filter(URAU_CATG == "L")

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

n3 <- ggplot(fua %>% dplyr::filter(n>10), aes(x = URAU_POPL, y = n)) +
  geom_point () +
  theme_light() +
  labs(x = "Population des aires urbaines fonctionnelles en 2006", 
       y = "Nombre de participations") +
  geom_smooth(method = 'lm')
n3


# Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(URAU_POPL) , data = fua %>% dplyr::filter(n > 10))
reg
summary(reg)
coeff=coefficients(reg)
# Equation de la droite de regression :
eqlin = paste0("y = ", round(coeff[2],2), " * x + ", round(coeff[1],1))
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

# Graphe
n2 + geom_abline(intercept = -0.54, slope = 0.38, color="red",
                 linetype="dashed", size=1.5) +
  #ggtitle(eq) + 
  annotate(geom="text", x=100000, y=250, label = paste0(eq, "\nR2 = 0.25"),
           color="blue")




