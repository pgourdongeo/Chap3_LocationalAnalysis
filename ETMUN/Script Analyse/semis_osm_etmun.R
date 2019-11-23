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
library(sf)
library(mapview)
library(skimr)
library(ggplot2)
library(ggspatial)
library(cowplot)
library(rosm)
library(purrr)


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

# library("rnaturalearth")
# library("rnaturalearthdata")
# 
# world <- ne_countries(scale = "medium", returnclass = "sf")
# world <- world %>% 
#   st_transform(crs = 3035)



## Planche Carto adaptative ------------------------------------

osm.types()

### FUNCTION ???
FacetPointCarto <- function(sf, facetvar, size, colourColumn, 
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
  
  plancheCarto <- cowplot::plot_grid(plotlist = g)
  return(plancheCarto)
}


sort(unique(ETMUN_Point$Network_Name))

samplenet <- ETMUN_Point %>%
  filter(Network_Name == "ACRplus" |
         Network_Name == "ACTE" |
         Network_Name == "EFUS" | # Vallée de Montmorency CA
         Network_Name == "OWHC")


sampleCarto <- FacetPointCarto(sf = samplenet,
                         facetvar = samplenet$Network_Name, 
                         size = 1, 
                         colourColumn = "EuropeYN")
sampleCarto



freq <- as.data.frame(table(ETMUN_Point$Network_Name, ETMUN_Point$EuropeYN))
freq <- freq %>% filter(Freq == 0)
eurVec <- as.character(sort(unique(freq$Var1)))

resdf <- data.frame(Network_Name = unique(ETMUN_Point$Network_Name))
resdf <- resdf %>% 
  mutate(GRP = c(rep(1:14, each = 4),5, 5, 5))

ETMUN_Point <- left_join(ETMUN_Point, resdf, "Network_Name")

netEur <- ETMUN_Point %>% filter(Network_Name %in% eurVec) #17
netWld <- ETMUN_Point %>% filter(!Network_Name %in% eurVec) #42

test <- ETMUN_Point %>% 
  group_by(GRP) %>% 
  group_map(FacetPointCarto(sf = ETMUN_Point,
                                  facetvar = samplenet$Network_Name, 
                                  size = 1, 
                                  colourColumn = "EuropeYN"))




Codegroup <- rep(1:5, each = 4)

eurVecdf <- data.frame(NAME = eurVec, GRP = character(17))
eurVecdf <- eurVecdf %>% 
  mutate(GRP = c(rep(1:4, each = 4),5))

Codegroup <- rep(1:5, each = 4)


netEur <- netEur %>% 
  mutate(grpRes = )
fournet <- seq(from = 1, to = length(eurVec), by = 4)

eurVec1 <- eurVec[fournet[1]:(fournet[2]-1)]
truc <- split(eurVec, fournet)


FacetPointCarto(sf = netEur,
                facetvar = samplenet$Network_Name, 
                size = 1, 
                colourColumn = "EuropeYN")

# cartoBis <- FacetPointCarto(sf = ETMUN_Point,
#                             facetvar = ETMUN_Point$Network_Name, 
#                             size = 0.3, 
#                             colourColumn = "EuropeYN")
# cartoBis


sf = ETMUN_Point %>% filter(Network_Name == "UBC")
facetvar = ETMUN_Point$Network_Name
size = 0.3
colourColumn = "EuropeYN"
g <- list()
for(i in facetvar){ 
  data <- sf %>% filter(facetvar == i)
  
  g[[i]] <- ggplot() +
    #ggspatial::annotation_map_tile(zoomin = TRUE) +
    guides(fill = TRUE) +
    ggtitle(i) +
    ylab('') +
    xlab('') +
    theme_light() +
    theme(axis.text = element_blank())
  
  if ( !is.na(colourColumn)) {
    g[[i]] <- g[[i]] +
      geom_sf(data = data, size = size, show.legend = "point", 
              aes_string(colour = colourColumn), alpha = alpha )
  } 
  else {
    g[[i]] <- g[[i]] +
      geom_sf(data = data, size = size, show.legend = FALSE, 
              alpha = alpha, colour = colourVector)
  }
}

#plancheCarto <- cowplot::plot_grid(plotlist = g)
plancheCarto <- theme_set(theme_cowplot(plot_grid(plotlist = g)))
return(plancheCarto)




















#ggsave(filename = "Semis des villes membres des associations en 2015.pdf",width = 295 ,height = 210, units = "mm",dpi = 300)
unique(ETMUN_Point$Network.Name)

tabeuropeYN <-table(samplenet$Network.Name, samplenet$EuropeYN)
tabeuropeYN
prop.table(tabeuropeYN,margin = 1)

### Carto simple

map1 <-ggplot() + 
  geom_sf(data = samplenet,
          size = samplenet$Pop_collect, 
          show.legend = "point") + 
  labs(caption = "Source : ETMUN database, 2015") + 
  #coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], datum = NA)+ 
  ggtitle("Semis des entités membres d'associations transnationales de municipalité")+
  # scalebar( x.min = bbox[1], x.max = bbox[3],y.min = bbox[2], y.max = bbox[4],location = "bottomright",   dist = 500,
  #           height = 0.005,st.size = 3)+ 
  theme_light() +
  facet_wrap(~Network_Name)
map1
