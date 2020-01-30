###############################################################################
#                            VILLES ET MEMBRES ETMUN
#
# DESCRIPTION : cartographie du poids des villes européennes dans le réseau ETMUN
# 
#
# PG, AD, Octobre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

## Library
library(tidylog)
library(skimr)
library(tidyverse)
library(sf)
library(mapview)
library(cartography)


## Load data
etmun <- readRDS("Data/ETMUN_Membership_GNid.RDS")
skim(etmun)

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")


## Prepare data
### rm na : removed 75 out of 17333 rows (<1%)
etmun <- etmun %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))

### transform ti sf
sfEtmun <- st_as_sf(etmun, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
   st_sf(sf_column_name = "geometry") %>%
   st_transform(crs = 3035)

### summarise data by cities
sfCities <- sfEtmun %>% 
  group_by(geonameId, asciiName) %>% 
  summarise(nbMembers = n())

### filter cities in Europe
sfCitiesEur <- st_intersection(sfCities, rec)

### quick view
mapview(sfCitiesEur)


##
propMap <- function(frame = rec, bgmap = sfEU, sfpts, var,
                    varLeg, titleLeg, labels, source) {
  
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5)
  propSymbolsLayer(sfpts, var = var, 
                   col = "#370028", inches = 0.2, 
                   border = "white", lwd = 0.2, symbols = "circle",
                   legend.pos = NA, add = TRUE)
  labelLayer(sfCitiesEur %>% top_n(n = 10), txt = "asciiName",
             col= "white", 
             cex = 0.6, 
             font = 1,
             halo = TRUE, 
             bg = "black",
             r = 0.05, 
             overlap = FALSE, 
             show.lines = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, add = TRUE)
  
  # Add legend
  legendCirclesSymbols(pos = c(1000000, 3000000),
                       cex = 1,
                       var = varLeg,
                       inches = 0.2, col = "#370028", border = "white", lwd = 0.2, 
                       title.txt = titleLeg, title.cex = 0.8, values.cex = 0.6, style = "e")
  
  # Add an explanation text
  text(x = 1000000, y = 2600000, labels = labels, cex = 0.7, adj = 0)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = source, 
              author = "PG, AD, 2019", 
              horiz = FALSE,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}



### Plot and save
# pdf(file = "OUT/test.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
# propMap(sfpts = sfCitiesEur %>% filter(nbMembers > 3),
#         var = "nbMembers",
#         varLeg = c(4, 10, 20, 40),
#         titleLeg = "Nombre de membres ETMUN",
#         labels = "Les villes comptant moins de 4 membres\nne figurent pas sur la carte",
#         source = "Source : ETMUN 2019")
# dev.off()

### transfo val
# sfCitiesEur <- sfCitiesEur %>% 
#   mutate(nbMembers2 = nbMembers*nbMembers)

pdf(file = "OUT/test2.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
propMap(sfpts = sfCitiesEur %>% filter(nbMembers > 3),
        var = "nbMembers2",
        varLeg = c(16, 100, 400, 1600),
        titleLeg = "Nombre de membres ETMUN (au carré)",
        labels = "Les villes comptant moins de 4 membres\nne figurent pas sur la carte",
        source = "Source : ETMUN 2019")
dev.off()



freq <- as.data.frame(table(sfCitiesEur$nbMembers))

# create barplots
distrib <- ggplot(data = sfCities, aes(x = nbMembers)) +
  geom_histogram() +
  scale_y_continuous(trans = "log10")

# display end save
pdf(file = "OUT/distrib.pdf", width = 8.3, height = 5.8)
distrib
dev.off()

