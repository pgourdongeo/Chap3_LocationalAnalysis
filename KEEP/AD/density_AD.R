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
library(ggplot2)


# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

# europe <- st_read("AD/SHP/NE/ne_10m_admin_0_countries_lakes.shp") %>% 
#    filter(REGION_UN == "Europe") %>% 
#    st_transform(crs = 3035)
europe <- st_read("../OtherGeometry/countries.geojson") %>% 
  st_transform(crs = 3035)
plot(st_geometry(europe))

sfEU <- st_read("AD/SHP/CNTR_RG_60M_2010.shp") %>% 
  st_transform(crs = 3035) %>% 
  st_cast("POLYGON") %>% 
  st_make_valid()

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)


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
plot_grid <- function(grid, adm, title, sources, bks, col){
  
  plot(st_geometry(europe), col = "ivory1", border = "ivory3", lwd = 0.5)
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = TRUE)
  plot(st_geometry(adm), col = NA, border = "black", lwd = 0.5, add = T)
  # Add a layout
  layoutLayer(title = title, 
              sources = sources, author = "PG, 2019", 
              theme = "blue.pal", 
              col = "darkred", coltitle = "white", 
              tabtitle = TRUE, 
              frame = TRUE, scale = 100, 
              posscale = "bottomleft",
              horiz = F)
}



# Cartography particiation/tessel

##  
par(mar = c(1, 4, 1, 4))

## 50 km cells
europegrided <- pt_in_grid(feat = sfPartner, adm = sfEU, cellsize = 50000)
skim(europegrided[[2]])

### defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
#cols <- carto.pal("sand.pal", length(bks) - 1)
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

plot_grid(grid = europegrided[[1]], adm = sfEU, 
          title = "Densité de participation à des projets de l'UE (date)", 
          sources = "Sources : ", 
          bks = bks, col = cols)

## Add legend
legendChoro(pos = "topleft", title.cex = 0.8,values.cex = 0.7,
            title.txt = "Nombre de participations\npar carreau de 2 500 km2", 
            breaks = bks, nodata = FALSE, values.rnd = 0, col = cols)





# Cartography participation/inhabitant(nuts2)

## import data nuts2
nuts23 <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()
  
#plot(st_geometry(nuts23))


## Intersect nuts and participations
inter <- st_intersects(nuts23, sfPartner)

## Count points in polygons
nuts23 <- st_sf(nuts23, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(nuts23))

# Display the map
par(mar = c(1, 4, 1, 4))
plot(st_geometry(nuts23), col = "ivory1", border = "ivory3",lwd =0.5,bg = "#FBEDDA")
propSymbolsLayer(nuts23, var = "n", inches = 0.1)
layoutLayer(title = "", sources="", author="", scale = NULL, tabtitle = TRUE, 
            frame=FALSE)

## Density
nuts23 <- nuts23 %>% 
  mutate(density = n / Pop_t_2001 * 10000)


## defines a set of breaks and colors
bks <- c(0.01,0.5,1, 1.5,3,6,10.5)
cols <- carto.pal("turquoise.pal", length(bks) - 1)

## Display the map
par(mar = c(1, 4, 1, 4))
plot(st_geometry(europe), col = "ivory1", border = "ivory3", lwd = 0.5)
choroLayer(nuts23, var = "density", border = NA, breaks= bks, col= cols, 
           legend.pos = "density", add = TRUE)
plot(st_geometry(nuts23), col = NA, border = "black", lwd = 0.2, add = T)

### Add legend
legendChoro(pos = "topleft", title.cex = 0.8,values.cex = 0.7,
            title.txt = "Nombre de participations\npour 10 000 habitants", 
            breaks = bks, nodata = TRUE, values.rnd = 0.5, col = cols)

### Add a layout
layoutLayer(title = "title", 
            sources = "sources :", author = "PG, 2019", 
            theme = "blue.pal", 
            col = "darkred", coltitle = "white", 
            tabtitle = TRUE, 
            frame = TRUE, scale = 100, 
            posscale = "bottomleft",
            horiz = F)




# Barplots participation/type of nuts
## participation rate in nuts
nuts23 <- nuts23 %>% 
  mutate(p = n * 100 / (sum(nuts23$n)))
## display barplots
p <- ggplot(data = nuts23, aes(x = Typo7, y = p)) +
  geom_bar(stat = "identity", width = 0.5) +
  theme_light()
  
p




# Visualisation participation/umz

## import data 
umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)


## Intersect umz and participations
inter <- st_intersects(umz, sfPartner)

## Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

## display barplot
n <- ggplot(data = umz, aes(x = n, y = Pop2011)) +
  geom_col () +
  theme_light() +
  labs(x = "Nombre de participations", y = "Population umz 2011")

n
