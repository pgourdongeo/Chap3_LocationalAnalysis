## Working directory huma-num
setwd("~/BD_Keep_Interreg")


# Library
library(tidyverse)
library(sf)
library(tidylog)
library(readr)
library(mapview)


# Import data
AdminEU <- st_read("OtherGeometry/ADMIN_SHAPE_UMZNaming/AdminNamingUMZ_LAU12_UA/LAU1_LAU2_UA_V3_[ETRS_1989_LAEA].shp")
AdminEU <- AdminEU %>% st_make_valid() #%>% st_cast(to = "POLYGON", group_or_split = F)
rec <- st_read("KEEP/AD/FDCARTE/rec_3035.geojson")

popGrid2006 <- read_delim("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2006/GEOSTAT_grid_EU_POP_2006_1K_V1_1_1.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
plot(AdminEU$geometry)

sfPopGrid2006 <- st_read("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2006/Grid_ETRS89_LAEA_1K_ref_GEOSTAT_2006.shp")

popGrid2011 <- read_delim("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2011/GEOSTAT_grid_POP_1K_2011_V2_0_1.csv", 
                          ",", escape_double = FALSE, trim_ws = TRUE)


sfPopGrid2011 <- st_read("GEOSTAT_PopGrid/GEOSTAT_PopGRID_2011/Grid_ETRS89_LAEA_1K-ref_GEOSTAT_POP_2011_V2_0_1.shp")

mapview(AdminEU)

# le work flow est simple :  
# 1. Prendre le fichier Admin Europe (réalisé en 2011 dans le cadre du Projet UMZ). 
# 2. Charger les grid population 2006 et 2011 (GEOSTAT_ Eurostat) 
# 3. Interpoler avec st_interpolate pour avoir 
# la population sur le fichier Admin





## Join pop attibute to sf
sfPopGrid2006 <- left_join(select(sfPopGrid2006, GRD_ID = GRD_INSPIR), popGrid2006, by = "GRD_ID")
sfPopGrid2011 <- left_join(select(sfPopGrid2011, GRD_ID), popGrid2011, by = "GRD_ID")



## interpolate Pop2006
inter2006 <- st_interpolate_aw(sfPopGrid2006[,"POP_TOT"], AdminEU, extensive = TRUE)
plot(inter2016$geometry)

inter2006df <- inter2006 %>% st_drop_geometry()

## joint result

AdminEU <- AdminEU %>% mutate(idInter06 = as.numeric(rownames(.)))

AdminEU <- AdminEU %>% left_join(inter2006df, by = c(  "idInter06" = "Group.1"))

AdminEU <- AdminEU %>% rename(PopAdmin06 = POP_TOT)


## interpolate Pop2011
inter2011 <- st_interpolate_aw(sfPopGrid2011[,"TOT_P"], AdminEU, extensive = TRUE)
plot(inter2011$geometry)

inter2011df <- inter2011 %>% st_drop_geometry()


# Joint result
AdminEU <- AdminEU %>% left_join(inter2011df, by = c( "idInter06" = "Group.1"))

AdminEU <- AdminEU %>% rename(PopAdmin11 = TOT_P)
## save
saveRDS(AdminEU, "CITY/AdminDelimPop0611.RDS")


## Fine file

AdminEU <- AdminEU %>%  mutate(COMM_ID = as.character(COMM_ID), NAME_ASCI = as.character(NAME_ASCI))


## Deal with paris arrondissements
AdminEU <- AdminEU %>% mutate(Paris = ifelse(str_detect(COMM_ID, "FR1175"),"FR75",COMM_ID))

Paris<- AdminFR %>% filter(Paris == "FR75") %>% group_by(Paris) %>% summarise(
                    PopAdmin06 = sum(PopAdmin06), PopAdmin11 = sum(PopAdmin11))

Paris <- Paris %>% mutate(COMM_ID = "FR1175056", NAME_ASCI = "Paris", idInter06 = NA)
Paris <- Paris %>% select(COMM_ID, NAME_ASCI, idInter06,PopAdmin06, PopAdmin11, geometry, Paris)


# Final file, Paris replacement
AdminEU <- AdminEU %>% filter(!str_detect(COMM_ID, "FR1175"))

AdminEU <- AdminEU %>%rbind(Paris)

## Final files
AdminEU <- AdminEU %>% select(-idInter06, -Paris)

saveRDS(AdminEU, "CITY/AdminDelimPop0611.RDS")

