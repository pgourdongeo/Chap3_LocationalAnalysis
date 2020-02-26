
##==========================================================================##         
##                             snap ETMUN - DO NOT RUN                      ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN / Rappatriement des points situés au            ##
##               large des côtes généralisées du shape Europe à l'intérieur ##
##               des polygones les plus proches                             ##
##                                                                          ##
##               Le fichier créé doit servir aux analyses spatiales         ##
##               impliquant l'intersection des participations avec le fond  ##
##               europe généralisé pour la carto ou les NUTS urbain/rural   ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##


# Working directory huma-num
# setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")

library(sf)
library(tidyverse)
library(tidylog)
library(mapview)
library(skimr)


# Import data

## old
# ETMUN <- read.csv2("DataSource/MembersETMUNGeocode.csv", 
#                    stringsAsFactors = F, 
#                    encoding = "UTF-8")

ETMUN <- readRDS("Data/ETMUN_Membership_GNid.rds")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)


# df to sf : removed 75 out of 17333 rows (<1%)
ETMUN <- ETMUN %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfETMUN <- st_as_sf(ETMUN, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)


# join etmun points to europe to have outsiders
outsiders <- sfETMUN %>%
  st_join(., select(sfEU, ID, NAME_EN, UE28)) %>%
  filter(is.na(ID))
#mapview(sfEU) + mapview(outsiders)


# function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
# Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

st_snap_points <-  function(x, y, max_dist) {
  
  if (inherits(x, "sf")) n = nrow(x)
  if (inherits(x, "sfc")) n = length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nrst = st_nearest_points(st_geometry(x)[i], y)
                  nrst_len = st_length(nrst)
                  nrst_mn = which.min(nrst_len)
                  if (as.vector(nrst_len[nrst_mn]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nrst[nrst_mn], "POINT")[2])
                })
  )
  return(out)
}

## Apply function
snap_outsiders <- st_snap_points(outsiders, sfEU, max_dist = 50000)

## add new coords to outsiders 
outsiders$geometry <- snap_outsiders
outsiders <- outsiders %>%  select(-ID, -NAME_EN, -UE28)

## join ousiders snaped to insiders
id <- outsiders$geonameId
sfETMUN <- sfETMUN %>% 
  filter(!geonameId %in% id) %>% 
  rbind(., outsiders)

### verif
outsiders <- sfETMUN %>%
  st_join(., select(sfEU, ID, NAME_EN, UE28)) %>%
  filter(is.na(ID))
mapview(sfEU) + mapview(outsiders)

## SAVE
saveRDS(sfETMUN, "Data/sfETMUN_snap.RDS")


