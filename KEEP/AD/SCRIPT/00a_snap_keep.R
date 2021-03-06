
##==========================================================================##         
##                             snap keep - DO NOT RUN                       ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base Eucicop/keep / Rappatriement des points situés au     ##
##               large des côtes généralisées du shape Europe à l'intérieur ##
##               des polygones les plus proches                             ##
##                                                                          ##
##               Le fichier créé doit servir aux analyses spatiales         ##
##               impliquant l'intersection des participations avec          ##
##               le fond europe ou les NUTS urbain/rural                    ##
##                                                                          ##
## PG, AD, Novembre 2019                                                    ##
##==========================================================================##


# Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")

# library
library(sf)
library(tidyverse)
library(tidylog)
library(mapview)
library(skimr)



# ==== load data ====
list.files("Data")

## LOAD PARTNERS
partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")
## LOAD BG MAP
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
## LOAD PARTICIPATIONS
participations <- readRDS("Data/Participations_All_Eucicop.RDS")
## LOAD PROJECTS
projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")


# ==== prepare data ====
# df to sf : removed 219 out of 58855 rows (<1%)
partners <- partners %>% 
  filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

sfPartners <- st_as_sf(partners, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

#mapview(sfEU) + mapview(sfPartners)



# ==== SNAP ====


## function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
## Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

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

## join partners to europe to have outsiders
outsiders <- st_join(sfPartners, select(sfEU, ID, NAME_EN, UE28)) %>% 
  filter(!duplicated(ID_PARTNER)) %>% 
  filter(is.na(ID))

mapview(sfEU) + mapview(outsiders)

## Apply function
snap_outsiders <- st_snap_points(outsiders, sfEU, max_dist = 50000)

## add new coords to outsiders 
outsiders$geometry <- snap_outsiders
outsiders <- outsiders %>%  select(-ID, -NAME_EN, -UE28)

## join ousiders snaped to sfPartner
id <- outsiders$geonameId
sfPartners <- sfPartners %>% 
  filter(!geonameId %in% id) %>% 
  rbind(., outsiders)

### verif
outsiders <- st_join(sfPartners, select(sfEU, ID, NAME_EN, UE28)) %>% 
  filter(!duplicated(ID_PARTNER)) %>% 
  filter(is.na(ID))
#mapview(sfEU) + mapview(outsiders)

rm(outsiders, id, snap_outsiders)


# ==== Prepare new sf for analyse scripts ====
# (combine participations - partners - projects)


## Add coords partners (with snaped points of outsiders) to participations
sfParticipations_snap <- participations %>% 
  left_join(sfPartners, by = "ID_PARTNER")
class(sfParticipations_snap)
sfParticipations_snap <- st_as_sf(sfParticipations_snap)
class(sfParticipations_snap)

sfParticipations_snap <- sfParticipations_snap %>% 
  filter(!is.na(st_dimension(geometry)))


### add period to participations
sfParticipations_snap <- sfParticipations_snap %>% 
  left_join(select(projects, Period, ID_PROJECT),  by = "ID_PROJECT")

## SAVE
#saveRDS(sfParticipations_snap, "Data/sfParticipations_snap.RDS")

