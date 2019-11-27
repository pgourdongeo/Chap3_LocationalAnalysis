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
library(ggsci)


# Import data
list.files("DataSource")
ETMUN <- read.csv2("DataSource/MembersETMUNGeocode.csv", 
                   stringsAsFactors = F, 
                   encoding = "UTF-8")

# prepare data
skim(ETMUN)

## rm na before transform to sf : removed 75 out of 17333 rows (<1%)
ETMUN <- ETMUN %>% filter_at(.vars = c("lon", "lat"), any_vars(!is.na(.)))
## Transform into sf
ETMUN_Point <- st_as_sf(ETMUN, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

## Column booleene Europe/NonEurop
ETMUN_Point <- ETMUN_Point %>% 
  mutate(EuropeYN = ifelse(Continent == "Europe", "Europe", "Other_Continent"))

skim(ETMUN_Point)

# ## select Networks according to their spatial extent (EUROPE/ALL WORLD)
# freq <- as.data.frame(table(ETMUN_Point$Network_Name, ETMUN_Point$EuropeYN))
# freq <- freq %>% filter(Freq == 0)
# eurVec <- as.character(sort(unique(freq$Var1)))
# 
# netEur <- ETMUN_Point %>% filter(Network_Name %in% eurVec) #17
# netWld <- ETMUN_Point %>% filter(!Network_Name %in% eurVec) #42



#osm.types()

# Functions
## add scale bar and sources 

## return all maps in a list (too long) 
PlotPoints <- function(sf, facetvar, size, colourColumn, 
                       colourVector = NA, alpha = 0.6 ){

  g <- list()
  
  for(i in facetvar){ 
    
    data <- sf %>% filter(facetvar == i)
    data_grp <- data %>%                        ## too looong, try with group_map
      group_by(Network_Name, EuropeYN) %>%
      summarise(nb_inEur = n()) %>%
      mutate(nb_T = sum(nb_inEur),
             P_inEur = round(nb_inEur * 100 /nb_T))
    val1 <- data_grp$nb_T
    val2 <- data_grp$P_inEur
    
    g[[i]] <- ggplot() +
      ggspatial::annotation_map_tile(type = "cartolight", zoomin = TRUE) +
      ggtitle(label = i, subtitle = paste(val1, " villes (",
                                          val2, " % en Europe)", sep = "")) +
      theme_void() +
      theme(plot.title = element_text(size = 10))
    
    if ( !is.na(colourColumn)) {
      g[[i]] <- g[[i]] +
        geom_sf(data = data, size = size, show.legend = FALSE, 
                aes_string(colour = colourColumn), alpha = alpha) +
        #ggspatial::annotation_scale(location = "br") +
        scale_colour_manual(values = c("#24325FFF", "#e66600")) +
        # annotate(geom = "text",
        #          label = paste("Nb villes = ", val1, "\n",
        #                        val2, " % en Europe", sep = ""),
        #          size = 2.5, hjust = 0, vjust = 1.5,
        #          x = -Inf, y = Inf) +
        theme(plot.title = element_text(size = 10),
              plot.subtitle = element_text(size = 8)) 
    } 
    else {
      g[[i]] <- g[[i]] +
        geom_sf(data = data, size = size, show.legend = FALSE, 
                alpha = alpha, colour = colourVector)
    }
  }
  return(g)
}
## return all grids of n plots in a list (sources don't work)
Gridplots <- function(myplots, n, sources){
  
  splitted_plots <- split(myplots, ceiling(seq_along(myplots)/n))
  
  plot_grid
  gridded_plots <- lapply(splitted_plots, 
                          function(x) plot_grid(plotlist = x) + 
                                      draw_text(sources, x = 1, y = 1, hjust = 0)) 

  return(gridded_plots)
  
  
}



# Planches pour chap3  ----------------------------------------
#dir.create("OUT/planche_chap3")

## Prepare data Europe
vecNetEur <- c("EixoAtlántico", "ENTP", "CAAC", "ACTE", 
               "RECEVIN", "GMF", "FAIC", "HYER")
myNetEur <- ETMUN_Point %>% filter(Network_Name %in% vecNetEur)


## Apply function 1
Points_List <- PlotPoints(sf = myNetEur,
                          facetvar = myNetEur$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")
## Apply function 2
Grid_list <- Gridplots(myplots = Points_List, n = 4,
                       sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA\nPG, AD, 2019")

## save
for (i in 1:length(Grid_list)) {
  save_plot(plot = Grid_list[[i]], filename = paste("OUT/planche_chap3/members_EUR_ETMUN_planche_", i, ".pdf", sep = ""),
            base_width = 8.3, base_height = 5.8)
}



## Prepare data world
vecNetWld <- c("ALDA", "AVEC", "EAHTR", "CNFE",
               "MedCities", "DigitalCities", "FMDV", "ECAD",
               "Climate Alliance", "LIKE", "REVES")
myNetWld <- ETMUN_Point %>% filter(Network_Name %in% vecNetWld)

## Apply function 1
Points_List <- PlotPoints(sf = myNetWld,
                          facetvar = myNetWld$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")
## Apply function 2
Grid_list <- Gridplots(myplots = Points_List, n = 4,
                       sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA\nPG, AD, 2019")

## save
for (i in 1:length(Grid_list)) {
  save_plot(plot = Grid_list[[i]], filename = paste("OUT/planche_chap3/members_WLD_ETMUN_planche_", i+2, ".pdf", sep = ""),
            base_width = 8.3, base_height = 5.8)
}



## prepare data EFUS
myNetEFUS <- ETMUN_Point %>% filter(Network_Name == "EFUS",
                                    EuropeYN != "Other_Continent") # rm 3

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")
myNetEFUS_centered <- st_intersection(myNetEFUS, rec) %>% select(-id) # rm 4

## Apply function 1
Points_List <- PlotPoints(sf = myNetEFUS_centered,
                          facetvar = myNetEFUS_centered$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")

ggsave(plot = Points_List[[1]], "OUT/planche_chap3/members_EFUS_ETMUN_planche_6.pdf",
       width = 8.3, height = 5.8)



# Planches pour chap2  ----------------------------------------
dir.create("OUT/planche_chap2")

## Prepare data Europe
vecNet <- c( "ICP", "UBC", "OWHC")
myNet <- ETMUN_Point %>% filter(Network_Name %in% vecNet)


## Apply function 1
Points_List <- PlotPoints(sf = myNet,
                          facetvar = myNet$Network_Name, 
                          size = 1, 
                          colourColumn = "EuropeYN")
## Apply function 2
Grid_list <- Gridplots(myplots = Points_List, n = 4,
                       sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA\nPG, AD, 2019")

## save
for (i in 1:length(Grid_list)) {
  save_plot(plot = Grid_list[[i]], filename = paste("OUT/planche_chap2/members_ETMUN_planche_", i, ".pdf", sep = ""),
            base_width = 8.3, base_height = 5.8)
}




# Planches Carto pour annexe  ------------------------------------
dir.create("OUT/planche_annexe")

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


