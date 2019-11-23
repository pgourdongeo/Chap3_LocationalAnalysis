
## Packages

library(tidyverse)

library(sf)

library(mapview)
library(skimr)
library(ggplot2)
library(ggspatial)

library(cowplot)


#### Load ETMUN

list.files("DATA_ETMUN")
ETMUN <- read.csv2("DATA_ETMUN/Master_CollecteCitiesNetworksV1_UTF8.csv", stringsAsFactors = F, encoding = "UTF-8")
skim(ETMUN)
ETMUN <- ETMUN %>% mutate(Y = as.numeric(Y), X = as.numeric(X))

#Note : refaire le géocodage automatiquement avec photon
ETMUN <- ETMUN %>% filter(!is.na(X))

## Transform into sf

ETMUN_Point <- st_as_sf(ETMUN, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

### Column booleene Europe NonEurop

ETMUN_Point <- ETMUN_Point %>% mutate(EuropeYN = ifelse(Continent == "Europe", "Europe", "Other_Continent"))
### Planche Carto adaptative

skim(ETMUN_Point)

FacetPointCarto <- function(sf, facetvar, size = 1, colourColumn = NA, colourVector = NA, alpha =0.7 ){
  g <- list()
  for(i in facetvar){ 
    data <- sf%>% filter(facetvar == i)
    
    g[[i]] <- ggplot() +
      ggspatial::annotation_map_tile(zoomin = T) +
      guides(fill=T)+
      ggtitle(i)+
      ylab('')+
      xlab('')+theme_light()+
      theme(axis.text = element_blank())
      
    if ( !is.na(colourColumn)) {
      g[[i]] <- g[[i]] +
        geom_sf(data = data,size =size, show.legend = "point", aes_string(colour = colourColumn), alpha = alpha )
    } else {
      g[[i]] <- g[[i]] +
        geom_sf(data = data,size =size,show.legend = F, alpha = alpha, colour =  colourVector)
    }
  }
 
  plancheCarto <- cowplot::plot_grid(plotlist = g)
  return(plancheCarto)
}

listcont<- unique(ETMUN_Point$Continent)
unique(ETMUN_Point$Network.Name)

sample <- sample_n(ETMUN_Point, 500)

samplenet <- ETMUN_Point %>% filter(Network.Name == "ICP" | Network.Name == "UBC" | Network.Name == "HERITAGE EUROPE" | Network.Name == "OWHC")

samplenet <- samplenet %>% mutate(EuropeYN = ifelse(is.na(EuropeYN), "Other_Continent", EuropeYN))
carto <- FacetPointCarto(sf = samplenet,facetvar = samplenet$Network.Name, size = samplenet$Pop.indicative,alpha =0.5 , colourColumn = "EuropeYN")
cartoBis <- FacetPointCarto(sf = ETMUN_Point,facetvar = ETMUN_Point$Network.Name, size = 0.3, colourColumn = "EuropeYN")
carto
cartoBis
ggsave(filename = "Semis des villes membres des associations en 2015.pdf",width = 295 ,height = 210, units = "mm",dpi = 300)
unique(ETMUN_Point$Network.Name)

tabeuropeYN <-table(samplenet$Network.Name, samplenet$EuropeYN)
tabeuropeYN
prop.table(tabeuropeYN,margin = 1)
### Carto simple

map1 <-ggplot() + 
  geom_sf(data = samplenet,size =samplenet$Pop.indicative, show.legend = "point")+ 
  labs( caption = "Source: ETMUN database, 2015")+ 
  #coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], datum = NA)+ 
  ggtitle("Semis des entités membres d'associations transnationales de municipalité")+
  # scalebar( x.min = bbox[1], x.max = bbox[3],y.min = bbox[2], y.max = bbox[4],location = "bottomright",   dist = 500,
  #           height = 0.005,st.size = 3)+ 
theme_light()+facet_wrap(~Network.Name)
map1


#### Planche Pop


Eurocities <- read_sf("DATA_ETMUN/BDNetwork_Eurocities_EnergyCities_3035.shp", 
                      coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

Eurocities <- Eurocities %>%
  select(-PopLog10)
Eurocities
Eurotowns <- read_sf("DATA_ETMUN/BdNetowrk_UBC_Citta_Eurotowns_3035.shp", 
                     coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)
Eurotowns <- Eurotowns %>% filter(`Network Na` == "Eurotowns" | `Network Na` == "CITTASLOW") %>% 
  select(-PopLog10)


Bdcomplete <- ETMUN_Point %>% select(-EuropeYN)%>% filter(!Network.Name == "CITTASLOW" & !Network.Name == "Energy Cities")
namecol <- colnames(Bdcomplete)

colnames(Eurocities) <- namecol
colnames(Eurotowns)<-namecol

test <- rbind(Bdcomplete, Eurotowns, Eurocities)
mapview(test)
ETMUN_Point

unique(test$Network.Name)
samplesize <- test %>% filter(Network.Name == "Eurocities" | Network.Name == "Eurotowns" | 
                               Network.Name == "CITTASLOW" | Network.Name == "Energy Cities")%>% 
  filter(Continent == "Europe")
#Map
list.files("DATA_ETMUN")
EuropeFont <- read_sf("DATA_ETMUN/Nuts0Europe_3035.shp", crs = 3035)

options(scipen = 999)

ggplot() + geom_sf(data =EuropeFont)+
  geom_sf(data = samplesize,show.legend = "point",aes(size = Pop.indicative), alpha = 0.5, colour =  "deepskyblue4")+ 
  labs( caption = "Source: ETMUN database, 2015")+ 
  #coord_sf(xlim = bbox[c(1,3)], ylim = bbox[c(2,4)], datum = NA)+ 
  # scalebar( x.min = bbox[1], x.max = bbox[3],y.min = bbox[2], y.max = bbox[4],location = "bottomright",   dist = 500,
  #           height = 0.005,st.size = 3)+ 
 
  labs(size="Population indicative\ndes LAUs en 2015")+
  theme_light() + 
  facet_wrap(~Network.Name)+scale_size_continuous(range = c(0.1,10),
                                                  breaks = c(5000, 50000, 
                                                             100000, 500000,8000000),
                                                  trans="sqrt" )


ggsave(filename = "Semis_ETMUN_PopLAU_4asso.pdf",width = 295 ,height = 210, units = "mm",dpi = 300)

#### PCT continent per Network

map1 <-ggplot() + annotation_map_tile(zoomin = T)+
  geom_sf(data = ETMUN_Point %>% filter(Network.Name =="ACTE"),
          size = 2, show.legend = "point", aes_string(colour = "Continent"))+ 
   theme(legend.position="none")

map1

map2  <-ggplot() + annotation_map_tile(zoomin = T)+
  geom_sf(data = ETMUN_Point %>% filter(Network.Name =="OWHC"),
          size = 2, show.legend = "point", aes_string(colour = "Continent"))+ 
 theme(legend.position="none")
map2
gtest <- list(map1,map2,lgd)

map1
test <- arrangeGrob(grobs = gtest)
get_legend(test)
plot(test)

lgd <- get_legend(map1 + theme(legend.position="bottom"))
lgd <- get_legend(map2)
lgd
plot_grid(plotlist = gtest)
