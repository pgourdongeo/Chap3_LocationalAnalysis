## Working directory huma-num
setwd("~/BD_Keep_Interreg/KEEP")


# Library
library(tidyverse)
library(sf)
library(tidylog)
library(readr)
library(mapview)


# Import data
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")

## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")


# Map participations/pop 2006/cell 
#================================================

# le work flow est simple :  
# 1. Prendre notre grille Europe. 
# 2. Charger la grid population 2006. 
# 3. Interpoler avec st_interpolate pour avoir 
# la population mais sur notre grille 
# (% de surfaces de petits carreaux = % de population dans nos grands carreaux) 
# 4. Faire le point in grid comme d'hab en gardant la pop 
# 5. Calculer un nombre de participations pour 100 000 hab 
# (ou 10 000 selon les ordres de grandeur) . 
# 6. Ploter

## load eurostat population grids
popGrid <- read_delim("DataSource/GEOSTAT_grid_EU_POP_1K/GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
sfPopGrid <- st_read("DataSource/GEOSTAT_grid_EU_POP_1K/Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.shp")


## Join pop attibute to sf
sfPopGrid <- left_join(select(sfPopGrid, GRD_ID = GRD_INSPIR), popGrid, by = "GRD_ID")

## transform to 3035
sfPopGrid <- sfPopGrid %>% 
  st_transform(crs = 3035)


## Create a regular grid in our spatial extent
grid <- st_make_grid(x = sfEU, cellsize = 50000, what = "polygons")
mapview(grid)

### Keep only cells that intersect lands
. <- st_intersects(grid, sfEU)
grid <- grid[sapply(X = ., FUN = length)>0]
mapview(grid)

## interpolate 
test <- st_interpolate_aw(sfPopGrid["POP_TOT"], grid, extensive = TRUE)


## save
saveRDS(test, "AD/SHP/interpolation.RDS")
