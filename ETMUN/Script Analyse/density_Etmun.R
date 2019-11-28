###############################################################################
#                         CARTOGRAPHIE DES DENSITES 
#
# DESCRIPTION : construction d'un carroyage sur l'europe élargie, comptage des 
# participations aux projets de l'UE par carreau, cartographie des densités d'adhésion
#
# PG, AD, Octobre 2019
##############################################################################


## Working directory huma-num
#setwd("~/BD_Keep_Interreg")

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
library(readr)
library(RColorBrewer)
library(mapview)



# Import data
# EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F) 
# 
# ## rm na before transform to sf : removed 75 out of 17333 rows (<1%)
# EtmunPoints <- EtmunPoints %>% filter_at(.vars = c("lon", "lat"), any_vars(!is.na(.)))
# 
# sfAdhesion <- st_as_sf(EtmunPoints, coords = c("lon", "lat"), crs = 4326) %>%
#   st_sf(sf_column_name = "geometry") %>%
#   st_transform(crs = 3035)

sfETMUN_snap <- readRDS("Data/sfETMUN_snap.RDS")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()



# Functions
#================================================

## Build a regular grid and count points in each cell
pt_in_grid <- function(feat, adm, cellsize){
  
  ptgrid <- list()
  
  # Create a regular grid (adm bbox)
  grid <- st_make_grid(x = adm, cellsize = cellsize, what = "polygons")
  
  # Keep only cells that intersect adm
  . <- st_intersects(grid, adm)
  grid <- grid[sapply(X = ., FUN = length)>0]
  
  # # cut cells in the borders
  # ptgrid[["grid"]] <- st_intersection(grid, adm)
  grid <- st_intersection(grid, adm)
  
  # Count pts in grid
  . <- st_intersects(grid, feat)
  grid <- st_sf(n = sapply(X = ., FUN = length), grid)
  
  # cut cells in the borders
  ptgrid[["grid"]] <- grid 
  
  # remove null values
  ptgrid[["grid0"]] <- grid %>% 
    filter(n != 0)
  
  return(ptgrid)
}

## Plot a gridded map
plot_grid <- function(grid, adm, frame, sources, titleLeg, labels){
  
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.5, add = T)
  
  ## Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8, 
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
  
  # Add an explanation text
  text(x = 1000000, y = 2700000, labels = labels, cex = 0.7, adj = 0)
  
  # Add a layout
  layoutLayer(title = "",
              sources = sources,
              author = "PG,AD, 2019",
              horiz = F,
              col = NA,
              frame = F,
              scale = 500,
              posscale = c(6500000, 1000000)
  )
  
}


## plot a points map
plot_points <- function(frame, adm, sf, txtLeg, source){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0))
  
  # Plot the map
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)], add = TRUE)
  
  # Add a legend
  legend(x = "left", 
         legend = txtLeg, 
         bty = "n",
         cex = 0.8)
  # Add a layout
  layoutLayer(title = "",
              sources = source,
              author = "PG, AD, 2019",
              horiz = TRUE,
              col = NA,
              frame = F,
              scale = 500,
              posscale = c(6500000, 1000000)
  )
}

## Plot a choro map
dens_map <- function(frame, bgmap, sf, titleLeg, sources, labels){
  
  # set the margins
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(frame), border = NA, lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5, add = TRUE)
  choroLayer(sf, var = "density", border = NA, breaks= bks, col= cols, 
             legend.pos = "density", add = TRUE)
  plot(st_geometry(sf), col = NA, border = "ivory4", lwd = 0.1, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8,
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
  
  # Add an explanation text
  text(x = 1000000, y = 2700000, labels = labels, cex = 0.7, adj = 0)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = sources, 
              author = "PG, AD, 2019", 
              horiz = FALSE,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}


# Map adhesions/cell 2019 
#================================================

## 50 km cells
europegrided <- pt_in_grid(feat = sfETMUN_snap, adm = sfEU, cellsize = 50000)

## visualize distribution
skim(europegrided[[1]])
hist(europegrided[[1]]$n)
## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Plot and save pdf
pdf(file = "OUT/europeGrid_etmunall.pdf", width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA", 
          titleLeg = "Nombre d'adhésions aux associations\nde municipalités par carreau de 2 500 km2*",
          labels = "*Discrétisation en progression\ngéométrique")

dev.off()

## PCT 0 participation : 72% de carreaux vides

summary(europegrided[[1]]$n)
sum(europegrided[[1]]$n)
skim(europegrided[[1]])
nrow(europegrided[[1]])
nrow(europegrided[[1]][europegrided[[1]]$n == 0,])/ nrow(europegrided[[1]]) *100




# density of adhesions by nuts
#================================================

## Prepare data
### Intersect nuts and participations
inter <- st_intersects(nutsUR, sfETMUN_snap)

### Count points in polygons
nutsUR <- st_sf(nutsUR, 
                n = sapply(X = inter, FUN = length), 
                geometry = st_geometry(nutsUR))

## Add density to df : nb of participations for 10 000 inhabitants
nutsUR <- nutsUR %>% 
  mutate(density = n / Pop_t_2001 * 100000)

## Display map

### distribution
skim(nutsUR$density)
hist(nutsUR$density)

distrib <- nutsUR %>% filter(density >= 1) 
distrib <- sort(distrib$density)
hist(distrib)
bks <- c(0, getBreaks(v =  distrib, method = "fisher-jenks", nclass = 6))
### defines a set of breaks and colors
# myvar <- nutsUR %>% filter(density > 0) 
# bks <- c(0, getBreaks(v =  myvar$density, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

### Plot and save
pdf(file = "OUT/density_nutsUR_etmunpall.pdf",width = 8.3, height = 5.8)
dens_map(frame = rec, 
         bgmap = sfEU, 
         sf = nutsUR, 
         titleLeg = "Nombre d'adhésions aux associations de municipalités\n par NUTs pour 100 000 habitants*",
         labels = "*Discrétisation selon les seuils naturels\n(fisher-jenks)",
         sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA")
dev.off()



# Barplots participations/type of nuts
#================================================

# average numbers of participations by type of nuts
nutsUR <- nutsUR %>% 
  group_by(Typo7) %>% 
  mutate(nbm = mean(n)) %>% 
  ungroup()

bibi <- data.frame(Typo7 = unique(nutsUR$Typo7), 
                   nbm = unique(nutsUR$nbm),
                   Lead = "Adhésions à des associations de municipalités")


# create barplots
projNuts <- ggplot(data = bibi, aes(x = reorder(Typo7, -nbm), y = nbm, fill = Lead)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(nbm)), position = position_dodge(0.9), vjust = 1.6, color = "white") +
  labs(x = "Types de NUTS",
       y = "Nombre moyen d'adhésions à des associations de municipalité") +
  scale_fill_manual(values= "#999999") +
  theme_light() +
  labs(caption = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA ; ESPON DB 2013\nPG, AD, 2019") +
  theme(legend.position = "none", legend.title = element_blank(),     
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        plot.caption = element_text(size = 6)) 

# display end save
pdf(file = "OUT/adh_nutsUR_etmunall.pdf", width = 8.3, height = 5.8)
projNuts
dev.off()

