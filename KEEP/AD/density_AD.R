###############################################################################
# NAME
# DESCRIPTION
# AUTHOR
# DATE
##############################################################################


setwd("~/git/Chap3_LocationalAnalysis/KEEP")


# Library
library(cartography)
library(dplyr)
library(sf)
library(tidylog)
library(skimr)
library(lwgeom)

# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

europe <- st_read("AD/SHP/NE/ne_10m_admin_0_countries_lakes.shp") %>% 
   filter(REGION_UN == "Europe") %>% 
   st_transform(crs = 3035)

sfEU <- st_read("AD/SHP/CNTR_RG_60M_2010.shp") %>% 
  st_transform(crs = 3035)%>% st_cast("POLYGON")


sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)


# Functions

## Build a regular grid and count points in each cell
pt_in_grid <- function(feat, adm, cellsize){
  # Create a regular grid (adm bbox)
  grid <- st_make_grid(x = adm, cellsize = cellsize, what = "polygons")
  
  # Keep only cells that intersect adm
  . <- st_intersects(grid, adm)
  grid <- grid[sapply(X = ., FUN = length)>0]
  
  # Count pts in grid
  . <- st_intersects(grid, feat)
  grid <- st_sf(n = sapply(X = ., FUN = length), grid)
  
  # cut cells in the borders
  adm <- st_make_valid(adm)
  grid <- st_intersection(grid, adm)
  
  return(grid)
}



## Plot a map
plot_grid <- function(grid, adm, title, bks, col){
  plot(st_geometry(adm), col = "ivory1", border = "ivory3", lwd = 0.5)
  choroLayer(europegrided, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n")
  plot(st_geometry(sfEU), add = TRUE, col = NA, border = "black", lwd = 0.5)
}

# Cartography

## remove null values
eg <- europegrided %>% 
  filter(n != 0)
skim(eg)
## defines a set of breaks and colors
## The getBreaks() function is used to classify the variable
bks <- c(0, getBreaks(v = eg$n, method = "geom", nclass = 6))
#cols <- carto.pal("sand.pal", length(bks) - 1)
cols <- c("#e5dddb", carto.pal("sand.pal", length(bks) - 2))

## 50 km cells
europegrided <- pt_in_grid(feat = sfPartner, adm = sfEU, cellsize = 50000)
plot_grid(grid = europegrided, adm = sfEU, title = "", bks = bks, col = cols)

## Add legend
legendChoro(pos = "topright", title.cex = 0.7,values.cex = 0.5, 
            title.txt = "Participations", 
            breaks = bks, nodata = FALSE, values.rnd = 0, col = cols)


######
nuts23 <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson")
