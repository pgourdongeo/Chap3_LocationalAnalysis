###############################################################################
#              CARTOGRAPHIE DES SEMIS DE POINTS DES ASSOCIATIONS
#                          exploration de la BD ETMUN
#
# DESCRIPTION : 
#
# PG, AD, Octobre 2019
##############################################################################

# Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")


# Packages
library(tidyverse)
library(tidylog)
library(sf)
library(mapview)
library(skimr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(rosm)
#library(purrr)


# Import data
list.files("DataSource")
ETMUN <- read.csv2("DataSource/MembersETMUNGeocode.csv", 
                   stringsAsFactors = F, 
                   encoding = "UTF-8")

# prepare data
skim(ETMUN)

## rm na before transform to sf
ETMUN <- ETMUN %>% filter(!is.na(lon))
## Transform into sf
ETMUN_Point <- st_as_sf(ETMUN, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

## Column booleene Europe/NonEurop
ETMUN_Point <- ETMUN_Point %>% 
  mutate(EuropeYN = ifelse(Continent == "Europe", "Europe", "Other_Continent"))

skim(ETMUN_Point)

## select Networks according to their spatial extent (EUROPE/ALL WORLD)
freq <- as.data.frame(table(ETMUN_Point$Network_Name, ETMUN_Point$EuropeYN))
freq <- freq %>% filter(Freq == 0)
eurVec <- as.character(sort(unique(freq$Var1)))

netEur <- ETMUN_Point %>% filter(Network_Name %in% eurVec) #17
netWld <- ETMUN_Point %>% filter(!Network_Name %in% eurVec) #42


## Planche Carto avec adaptation du zoom en fonction de l'emprise  ------------------------------------

#osm.types()

# Functions
## return all maps in a list (too long for netwld)
PlotPoints <- function(sf, facetvar, size, colourColumn, 
                       colourVector = NA, alpha = 0.7 ){
  g <- list()
  for(i in facetvar){ 
    
    data <- sf %>% filter(facetvar == i)
    
    g[[i]] <- ggplot() +
      ggspatial::annotation_map_tile(type = "cartolight", zoomin = TRUE) +
      guides(fill = TRUE) +
      ggtitle(i) +
      theme_void() +
      theme(plot.title = element_text(size = 10))
    
    if ( !is.na(colourColumn)) {
      g[[i]] <- g[[i]] +
        geom_sf(data = data, size = size, show.legend = FALSE, 
                aes_string(colour = colourColumn), alpha = alpha )
    } 
    else {
      g[[i]] <- g[[i]] +
        geom_sf(data = data, size = size, show.legend = FALSE, 
                alpha = alpha, colour = colourVector)
    }
  }
  
  return(g)
}
## return all grids of n plots in a list 
Gridplots <- function(myplots, n){
  
  splitted_plots <- split(myplots, ceiling(seq_along(myplots)/n))
  
  gridded_plots <- lapply(splitted_plots, function(x) plot_grid(plotlist = x))
  
  return(gridded_plots)
  
  # i <- unique(ceiling(seq_along(myplots)/n))
  # ggsave(filename = paste("OUT/members_EUR_ETMUN_planche", i, ".pdf", sep = " "),
  #        width = 8.3, height = 5.8)
}

## Apply function 1
Points_List <- PlotPoints(sf = netEur,
                          facetvar = netEur$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")
## Apply function 2
Grid_list <- Gridplots(myplots = Points_List, n = 4)

## Display
Grid_list


## Apply in 2 times for netwld
netWld1 <- netWld %>% filter(Network_Name != "Covenant of Mayors")
## Apply function 1
Points_List <- PlotPoints(sf = netWld1,
                          facetvar = netWld1$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")
## Apply function 2
Grid_list <- Gridplots(myplots = Points_List, n = 4)
## Display
Grid_list[1:5]
Grid_list[6:11]



## Apply in 2 times for netwld
netWld2 <- netWld %>% filter(Network_Name == "Covenant of Mayors")

## Function for a simple map
PlotPointsMayors <- function(sf, facetvar, size, colourColumn, title, 
                             colourVector = NA, alpha = 0.7 ){
  
  g <- ggplot() +
    ggspatial::annotation_map_tile(type = "cartolight", zoomin = TRUE) +
    guides(fill = TRUE) +
    labs(title = title) +
    theme_void() +
    theme(plot.title = element_text(size = 10)) +
    geom_sf(data = sf, size = size, show.legend = FALSE, 
            aes_string(colour = colourColumn), alpha = alpha)
}
## Apply function 1
Points <- PlotPointsMayors(sf = netWld2,
                          facetvar = netWld2$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN",
                          title = "Covenant of Mayors")

## Display
Points


### TO DO ---------------
# count nb of points for each network and display the n value on maps

