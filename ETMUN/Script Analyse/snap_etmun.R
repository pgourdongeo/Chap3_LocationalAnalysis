###############################################################################
#                               snap etmun
#                       
#                         
#
# DESCRIPTION : déplacement des points à l'intérieur des polygones sfEU/NUTSUR
#
# PG, AD, Novembre 2019
##############################################################################


## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")



## DO NOT RUN ---------------------------------------------
### import
# Import data
ETMUN <- read.csv2("DataSource/MembersETMUNGeocode.csv", 
                   stringsAsFactors = F, 
                   encoding = "UTF-8")

# df to sf : removed 75 out of 17333 rows (<1%)
ETMUN <- ETMUN %>% filter_at(.vars = c("lon", "lat"), any_vars(!is.na(.)))

sfETMUN <- st_as_sf(ETMUN, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)


### join etmun points to europe to have outsiders
sfETMUN_joinEU <- st_join(sfETMUN, select(sfEU, ID, NAME_EN, UE28))
outsiders <- sfETMUN_joinEU %>% filter(is.na(ID))
mapview(sfEU) + mapview(outsiders)

### function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
### Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

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

### Apply function
snap_outsiders <- st_snap_points(outsiders, sfEU, max_dist = 20000)

### check results
mapview(sfEU) + mapview(snap_outsiders, col.regions = "red")


### add new coords to outsiders 
outsiders$geometry <- snap_outsiders
outsiders <- outsiders %>%  select(-ID, -NAME_EN, -UE28)
#snap_outsiders_in <- cbind(df_outsiders, snap_outsiders)
#snap_outsiders_in <- st_as_sf(snap_outsiders_in)
class(outsiders)

### join ousiders snaped to sfETMUN
sfETMUN_inEU <- sfETMUN_joinEU %>% filter(!is.na(ID)) %>% select(-ID, -NAME_EN, -UE28)
sfETMUN_outsiders_snaped <- rbind(sfETMUN_inEU, outsiders)
class(sfETMUN_outsiders_snaped)

### verif
mapview(sfEU) + mapview(sfETMUN_outsiders_snaped)
### join partners to europe to check outsiders (>20km) numbers
test <- st_join(sfETMUN_outsiders_snaped, select(sfEU, ID, NAME_EN, UE28)) 
test_outsiders <- test %>% filter(is.na(ID))
mapview(sfEU) + mapview(test_outsiders)

## SAVE
saveRDS(sfETMUN_outsiders_snaped, "Data/sfETMUN_snap.RDS")

bibi <- readRDS("Data/sfETMUN_snap.RDS")

