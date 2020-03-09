## Working directory huma-num
setwd("~/BD_Keep_Interreg")


# Library
library(tidyverse)
library(sf)
library(tidylog)
library(readr)
library(mapview)
library(lwgeom)
library(skimr)
# Import data
AdminEU <- st_read("OtherGeometry/ADMIN_SHAPE_UMZNaming/AdminLAUEurope_Liliane05032020/LAU2017_LAU1_PT_EL_CY_SettlementUK_WGS84_REV1.shp",
                   stringsAsFactors = F )
AdminEU <- AdminEU %>% st_transform(crs = 3035)
AdminEU <- AdminEU %>% st_make_valid()
rec <- st_read("KEEP/AD/FDCARTE/rec_3035.geojson")

popGrid2006 <- read_delim("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2006/GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)


sfPopGrid2006 <- st_read("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2006/Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.shp", 
                         stringsAsFactors = F)

popGrid2011 <- read_delim("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2011/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv", 
                          ",", escape_double = FALSE, trim_ws = TRUE)


sfPopGrid2011 <- st_read("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2011/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp", 
                         stringsAsFactors = F)




# le work flow est simple :  
# 1. Prendre le fichier Admin Europe (réalisé en 2011 dans le cadre du Projet UMZ). 
# 2. Charger les grid population 2006 et 2011 (GEOSTAT_ Eurostat) 
# 3. Interpoler avec st_interpolate pour avoir 
# la population sur le fichier Admin





## Join pop attibute to sf
sfPopGrid2006 <- left_join(select(sfPopGrid2006, GRD_ID = GRD_INSPIR), popGrid2006, by = "GRD_ID")
sfPopGrid2011 <- left_join(select(sfPopGrid2011, GRD_ID), popGrid2011, by = "GRD_ID")

skim(sfPopGrid2011)

## interpolate Pop2006
inter2006 <- st_interpolate_aw(sfPopGrid2006["POP_TOT"], AdminEU, extensive = TRUE)


inter2006df <- inter2006 %>% st_drop_geometry()

## joint result

AdminEU <- AdminEU %>% mutate(idInter = as.numeric(rownames(.)))

AdminEU <- AdminEU %>% left_join(inter2006df, by = c( "idInter" = "Group.1"))

AdminEU <- AdminEU %>% rename(PopAdmin06 = POP_TOT)


## interpolate Pop2011
inter2011 <- st_interpolate_aw(sfPopGrid2011["TOT_P"], AdminEU, extensive = TRUE)


inter2011df <- inter2011 %>% st_drop_geometry()


# Joint result
AdminEU <- AdminEU %>% left_join(inter2011df, by = c( "idInter" = "Group.1"))

AdminEU <- AdminEU %>% rename(PopAdmin11 = TOT_P)
## save
saveRDS(AdminEU, "CITY/Data/AdminDelimPop0611.RDS")


## Fine file


## Deal with paris arrondissements
AdminEU <- AdminEU %>% mutate(Paris = ifelse(str_detect(Code_2, "FR75"),"YES",Code_2))

Paris<- AdminEU %>% filter(Paris == "YES") %>% group_by(Paris) %>% summarise(
                    PopAdmin06 = sum(PopAdmin06), PopAdmin11 = sum(PopAdmin11))

Paris <- Paris %>% mutate(Code_2 = "FR75056", Name_2 = "Paris", idInter = NA)
Paris <- Paris %>% select(Code_2, Name_2,PopAdmin06, PopAdmin11, geometry)

mapview(Paris)
# Final file, Paris replacement
AdminEU <- AdminEU %>% filter(!str_detect(Code_2, "FR75")) %>% select(-idInter)

AdminEU <- AdminEU %>%rbind(Paris)

## Final files
AdminEU <- AdminEU %>% select(-idInter,-Paris)

saveRDS(AdminEU, "CITY/Data/AdminDelimPop0611.RDS")

mapview(AdminEU %>% filter(str_detect(Code_2, "FR")))

skim(AdminEU)

missingpop <- AdminEU %>% filter(is.na(PopAdmin06) & is.na(PopAdmin11))
mapview(missingpop)
