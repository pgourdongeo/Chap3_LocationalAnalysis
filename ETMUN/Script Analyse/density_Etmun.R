###############################################################################
#                         CARTOGRAPHIE DES DENSITES 
#
# DESCRIPTION : construction d'un carroyage sur l'europe élargie, comptage des 
# participations aux projets de l'UE par carreau, cartographie des densités d'adhésion
#
# PG, AD, Octobre 2019
##############################################################################

## Working directory huma-num
setwd("~/BD_Keep_Interreg")

# setwd("~/git/Chap3_LocationalAnalysis/KEEP")
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
#library(ggpubr)
#library(GGally)


# Import data
list.files("KEEP/AD/FDCARTE")

sfEU <- st_read("KEEP/AD/FDCARTE/fondEurope.geojson" , crs = 3035) %>% 
  st_make_valid()

EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

Partner2 <- read_delim("DataSource/PartnersIDProj.csv",
                       ";", escape_double = FALSE, locale = locale(),
                       trim_ws = TRUE)

Projects <- read.table("DataSource/ProjectsID.csv", 
                       sep=";", dec=".", 
                       #quote="",
                       header = TRUE,
                       encoding="UTF-8")

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
plot_grid <- function(grid, adm, frame, sources, bks, col, titleLeg){
  
  bb <- st_bbox(frame)
  #c(bottom, left, top, right)
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

## Plot 3 maps with a unique legend
plot_grids <- function(grid1, grid2, grid3, 
                       title1, title2, title3,
                       adm, frame, sources, bks, col, titleLeg){
  
  bb <- st_bbox(frame)
  
  ## plot 3 maps on a single figure 
  par(mar = c(0, 0, 0, 0), mfrow = c(2, 2), ps=15)
  
  ## plot1
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
  # Add title
  mtext(text = title1,
        font = 2,
        side = 3, 
        line = -1, 
        adj = 0,
        cex =0.6)
  
  ## Add legend
  legendChoro(pos = c(1000000, 3000000),
              title.cex = 0.65,
              values.cex = 0.55,
              title.txt = titleLeg,
              breaks = bks,
              nodata = FALSE,
              values.rnd = 0,
              col = cols)
  
  
  ## plot2
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
  # Add title
  mtext(text = title2,
        font = 2,
        side = 3,
        line = -1,
        adj = 0,
        cex =0.6)
  
  
  ## plot3
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
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



# Map participations/cell 2000-2018

## 50 km cells
europegrided <- pt_in_grid(feat = sfPartner, adm = sfEU, cellsize = 50000)

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Plot and save pdf
#pdf(file = "AD/OUT/europeGrid_eucicopall.pdf",width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2018 ", 
          bks = bks, 
          col = cols, 
          titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2")
dev.off()



# Maps participations/cell 2000-2006, 2007-2013 et 2014-2020

## Prepare data
### add period to participations
partPeriod <- left_join(x = select(Partner2, ID_PARTICIPATION, ID_PARTNER, ID_PROJECT, lon, lat),
                        y = select(Projects, Period, ID_PROJECT),
                        by = "ID_PROJECT")

### tibble to sf
sfPartPeriod <- st_as_sf(partPeriod, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)


## defines a unique set of breaks for all maps (same legend as the 2000-2018 map)
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))


## prepare grids
europegrided1 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2000-2006"), 
                            adm = sfEU, cellsize = 50000)
europegrided2 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2007-2013"), 
                            adm = sfEU, cellsize = 50000)
europegrided3 <- pt_in_grid(feat = sfPartPeriod %>% filter(Period == "2014-2020"), 
                            adm = sfEU, cellsize = 50000)

## to do : % empty cells
# 7266 carreaux
hist(europegrided1[[1]]$n)
dplyr::filter(europegrided1[[1]], n == 0)
skim(europegrided1[[1]])

## display maps and save pdf
#pdf(file = "AD/OUT/europeGridPeriod_eucicopall.pdf", width = 8.3, height = 5.8)
plot_grids(grid1 = europegrided1[[1]], 
           grid2 = europegrided2[[1]],
           grid3 = europegrided3[[1]],
           adm = sfEU,
           frame = rec,
           sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2018\nPG, AD, 2019", 
           bks = bks, 
           col = cols, 
           title1 = "2000-2006",
           title2 = "2007-2013",
           title3 = "2014-2020",
           titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2")
dev.off()


