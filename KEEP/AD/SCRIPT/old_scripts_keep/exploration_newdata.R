###############################################################################
#                         Exploration de la BD KEEP
#                       livraison du 23 novembre 2019
#                         
#
# DESCRIPTION : exploration de la BD après le géocodage
#
# PG, AD, Novembre 2019
##############################################################################


## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")

library(readr)
library(sf)
library(tidyverse)
library(tidylog)
library(mapview)
library(skimr)

# load data
list.files("Data")

## LOAD PARTNERS

### doesn't work with readr nor read.csv2 / type of var pb
# partners <- read_delim("Data/UniquePartners_GNid_Eucicop.csv", 
#                        ";", 
#                        escape_double = FALSE, 
#                        col_types = cols(Department = col_character(),
#                                         geonameId = col_character(), 
#                                         lat_GN = col_character(),
#                                         lng_GN = col_character()), 
#                        trim_ws = TRUE)
# partners$lng_GN <- as.numeric(partners$lng_GN) 

### does work in rds format
partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")



# df to sf : removed 219 out of 58855 rows (<1%)
partners <- partners %>% 
  filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfPartners <- st_as_sf(partners, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

mapview(sfEU) + mapview(sfPartners)


## VERIF outsiders europe
GNCity <- readRDS("Data/GNid_uniqueCity_Eucicop.RDS")
GN_ID_ISO <- GNCity %>% 
  select(geonameId, countryCode)
skim(GN_ID_ISO)
GN_ID_ISO <- GN_ID_ISO %>% 
  filter(!is.na(geonameId))

sfPartners <- sfPartners %>% 
  left_join(GN_ID_ISO, by = "geonameId") 

bibi <- sfPartners %>% 
  filter(CountryISO != countryCode)

mapview(bibi)

skim(partners)

## VERIF if points in water with generalized sfEU 
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
mapview(sfEU) + mapview(sfPartners)

truc <- st_join(sfEU, sfPartners)
mapview(sfEU) + mapview(truc)

## DO NOT RUN ---------------------------------------------
### import
partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

# df to sf : removed 219 out of 58855 rows (<1%)
partners <- partners %>% 
  filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfPartners <- st_as_sf(partners, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

### join partners to europe to have outsiders
sfPartners_joinEU <- st_join(sfPartners, select(sfEU, ID, NAME_EN, UE28)) %>% 
  filter(!duplicated(ID_PARTNER))
outsiders <- sfPartners_joinEU %>% filter(is.na(ID))

### function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
### Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

st_snap_points = function(x, y, max_dist) {
  
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

### Apply function
snap_outsiders <- st_snap_points(outsiders, sfEU, max_dist = 20000)

### check results
mapview(sfEU) + mapview(snap_outsiders, col.regions = "red")
mapview(sfEU) + mapview(outsiders)

### bind new coords to outsiders (first remove former geometry)
df_outsiders <- outsiders %>% as.data.frame() %>%  select(-geometry, -ID, -NAME_EN, -UE28)
snap_outsiders_in <- cbind(df_outsiders, snap_outsiders)
snap_outsiders_in <- st_as_sf(snap_outsiders_in)
class(snap_outsiders_in)

### join ousiders snaped to sfPartner
sfPartners_inEU <- sfPartners_joinEU %>% filter(!is.na(ID)) %>% select(-ID, -NAME_EN, -UE28)
sfPartners_outsiders_snaped <- rbind(sfPartners_inEU, snap_outsiders_in)
#sfPartners_outsiders_snaped <- bind_rows(sfPartners_inEU, snap_outsiders_in)
class(sfPartners_outsiders_snaped)

### verif
mapview(sfEU) + mapview(sfPartners_outsiders_snaped)
### join partners to europe to check outsiders (>20km) numbers
test <- st_join(sfPartners_outsiders_snaped, select(sfEU, ID, NAME_EN, UE28)) %>% 
  filter(!duplicated(ID_PARTNER))
test_outsiders <- test %>% filter(is.na(ID))
mapview(sfEU) + mapview(test_outsiders)


## LOAD PARTICIPATIONS
participations <- readRDS("Data/Participations_All_Eucicop.RDS")
## Add coords partners (with snaped points of outsiders) to participations
sfParticipations_snap <- participations %>% 
  left_join(sfPartners_outsiders_snaped, by = "ID_PARTNER")
class(sfParticipations_snap)
sfParticipations_snap <- st_as_sf(sfParticipations_snap)
class(sfParticipations_snap)

sfParticipations_snap <- sfParticipations_snap %>% 
  filter(!is.na(st_dimension(geometry)))

## LOAD PROJECTS
projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")

### add period to participations
sfParticipations_snap <- sfParticipations_snap %>% 
  left_join(select(projects, Period, ID_PROJECT),  by = "ID_PROJECT")

## SAVE
saveRDS(sfParticipations_snap, "sfParticipations_snap.RDS")
bibi <- readRDS("sfParticipations_snap.RDS")
# st_write(sfParticipations_snap, "shp/sfParticipations_snap.shp")
# bibi <- st_read("shp/sfParticipations_snap.shp", crs = 3035)

## END ---------------------------------------------


## LOAD PARTICIPATIONS
participations <- readRDS("Data/Participations_All_Eucicop.RDS")

## Add coords partners to participations
participations <- participations %>% 
  left_join(partners, by = "ID_PARTNER")

# df to sf : removed 279 out of 84408 rows (<1%)
participations <- participations %>% 
  filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfParticipations <- st_as_sf(participations, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)



## LOAD PROJECTS
projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")

### add period to participations
sfParticipations <- sfParticipations %>% 
  left_join(select(projects, Period, ID_PROJECT),  by = "ID_PROJECT")

## SAVE with all join
saveRDS(sfParticipations, "sfParticipations_full.RDS")

