###############################################################################
#               ANALYSE DE DENSITE des semis ETMUN
#
# DESCRIPTION : 
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
library(sp)
library(tidylog)
library(skimr)
library(lwgeom)
library(ggplot2)
library(ggrepel)
library(spatstat)
library(maptools)


# Import data


EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F) 
EtmunPoints <- EtmunPoints %>% filter(!is.na(lon))

sfAdhesion <- st_as_sf(EtmunPoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)



sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")


# https://training.fws.gov/courses/references/tutorials/geospatial/CSP7304/documents/PointPatterTutorial.pdf
X_sf <- sfAdhesion %>% filter(Network_Name == "EAHTR")
#X_sf <- sfAdhesion 
X <- as(X_sf, 'Spatial')
X <- as.ppp.SpatialPoints(X)
summary(X)

par(mar = c(0,0,0,0))
plot(density(X))
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)

contour(density(X))
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)


#
env <- st_convex_hull(st_union(X_sf))
plot(density(X))
plot(env, add = TRUE)
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)



library(sp)
library(rgdal)
library(geosphere)
# use the distm function to generate a geodesic distance matrix in meters
sfAdhesion <- st_as_sf(EtmunPoints, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry")
X_sf <- sfAdhesion %>% filter(Network_Name == "EAHTR")
X <- as(X_sf, 'Spatial')
mdist <- distm(X)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 40 m
d=500000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
X$clust <- cutree(hc, h=d)

library(dismo)
library(rgeos)

# expand the extent of plotting frame
X@bbox[] <- as.matrix(extend(extent(X),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(X$clust))
for (i in 1:max(X$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(X, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

# plot
plot(ci@polygons, axes=F)
plot(X, col=rainbow(4)[factor(X$clust)], add=T)
plot(env, add = TRUE)
plot(st_geometry(X_sf), add = TRUE)
plot(st_geometry(sfEU), add = TRUE)
