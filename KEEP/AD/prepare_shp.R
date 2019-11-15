###############################################################################
#                       PREPARATION SHAPE EUROPE
#
# Création du fond de carte européen (UE + Turquie et Russie + Etats limitrophes)
# pour la cartographie des densités
# AD
# Octobre 2019
##############################################################################


setwd("~/git/Chap3_LocationalAnalysis/KEEP")

library(tidylog)
library(sf)
library(lwgeom)


# Le shape des pays du monde provient de Natural Earth :
# (https://www.naturalearthdata.com/downloads/110m-cultural-vectors/110m-admin-0-countries/)
# Les contours des pays de l'UE ont été retravaillé sous QGis pour qu'ils correspondent 
# à la généralisation du shape des Nuts issu d'une autre source. Le contour des autres pays limitrophes
# a également été retravaillé pour obtenir un niveau de généralisation uniforme. 

monde <- st_read("AD/FDCARTE/countriesAdm_ne_ad.geojson") %>% 
  st_make_valid()
rec <- st_read("AD/FDCARTE/rec_3035.geojson") 

plot(st_geometry(monde))
plot(st_geometry(rec), add = T)

europe <- st_intersection(monde, rec)
plot(st_geometry(europe))

st_write(europe, "AD/FDCARTE/fdEurope_3035.geojson")


europe <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035)




# semis de point KEEP - save to qgis
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

st_write(sfPartner, "AD/FDCARTE/sfPartner_3035.geojson")

# Création du shape sfPartnerSpe pour le comptage des points dans grille régulière
# Rappatriement des points situés au large des côtes généralisées du shape Europe
# effectué sous QGis (les points hors Europe non pas été corrigés)

sfPointsCorr <- st_read("AD/FDCARTE/sfPartner_inwaterGNutsUR.geojson")
idvec <- sfPointsCorr$ID_PARTICIPATION
sfPartnerSpe <- sfPartner %>% 
  filter(!ID_PARTICIPATION %in% idvec) %>% 
  rbind(., sfPointsCorr)
st_write(sfPartnerSpe, "AD/FDCARTE/sfPartner_3035_toGrid.geojson")
