
##==========================================================================##         
##                                                 ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN /              ##
##                                                                          ##
## PG, AD, février 2020                                                     ##
##==========================================================================##


## Working directory huma-num
#setwd("~/BD_Keep_Interreg/")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

# Library
library(tidylog)
library(tidyverse)
library(sf)
# library(cartography)
# 
# library(skimr)
# library(lwgeom)
# library(ggplot2)
# library(ggrepel)
# library(spatstat)
#library(sp)
# 


# Import data

ETMUN <- read.csv2("Data/ETMUN_Membership_GNid.csv", stringsAsFactors = F) 
# df to sf : removed 75 out of 17333 rows (<1%)
ETMUN <- ETMUN %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfETMUN <- st_as_sf(ETMUN, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)




## convexe hull

### pour un réseau
X_sf <- sfETMUN %>% filter(Network_Name == "EAHTR")
X_env <- st_convex_hull(st_union(X_sf))
st_area(X_env)

### plot
par(mar = c(0,0,0,0))
plot(X_env)
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)

### pour chacun des réseaux
make_hull <- function(sf){
  convex <- st_sf(st_convex_hull(st_union(sf)))
  convex <- convex %>% 
    rename(geometry = 'st_convex_hull.st_union.sf..') %>% 
    mutate(area = st_area(.),
           areaKM2 = round(area/1e+6))
  return(convex)
} 
net_hull <- sfETMUN %>% 
  group_by(Network_Name) %>% 
  do((make_hull(sf=.))) %>% 
  st_sf()

plot(net_hull %>% filter(Network_Name == "EAHTR"))


## densité et mode
### ggplot
ggplot(data = ETMUN %>% filter(Network_Name == "EAHTR"),
       aes(x = lng_GN, y = lat_GN)) +
  geom_density_2d(show.legend = TRUE) +
  geom_point() 



# https://training.fws.gov/courses/references/tutorials/geospatial/CSP7304/documents/PointPatterTutorial.pdf

library(maptools)

X_sf <- sfETMUN %>% filter(Network_Name == "EAHTR")
X <- X_sf %>% 
  as(., 'Spatial') %>% 
  as.ppp.SpatialPoints(.)


truc <- summary(X)
truc$intensity

par(mar = c(1,1,1,1))
plot(density(X))
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)

contour(density(X))
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)

K3 <- density(X, kernel = "disc", 
              sigma=50) # Using a 50km bandwidth
K3 <- density(X, kernel = "disc")
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)
plot(st_geometry(sfEU), add = TRUE)
plot(st_geometry(X_sf), add = TRUE)


#https://mgimond.github.io/Spatial/point-pattern-analysis-in-r.html#density-based-analysis-1








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
