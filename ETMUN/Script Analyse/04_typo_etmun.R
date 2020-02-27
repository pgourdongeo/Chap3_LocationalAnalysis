
##==========================================================================##         
##                        TYPOLOGIE DEs RESEAUX ETMUN                       ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Préparation des données en vue d'une typologie (CAH)       ##
##               cartographie des résidus                                   ##
##                                                                          ##
## PG, AD, février 2020                                                     ##
##==========================================================================##


# Working directory huma-num
#setwd("~/BD_Keep_Interreg/")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

# Library
library(tidylog)
library(tidyverse)
library(skimr)
library(sf)



# Load data
etmun <- readRDS("Data/ETMUN_Membership_GNid.RDS")
uniqueGN <- readRDS("Data/UniqueGNforETMUN.RDS")


# Creation of a new table of ETMUN NETWORKS (with number of city in each network)
network <- etmun %>% 
  group_by(Network_Name, Code_Network) %>% 
  summarise(Ncities = length(unique(geonameId))) 

# Add population to the db etmun
etmun <- left_join(etmun, select(uniqueGN, geonameId, population), by = "geonameId")
etmun$population <- as.numeric(etmun$population)

# % of cities by city size (category) in each project

## First, create a new vars: class of city size (1, 2, 3 ou 4) 
## si classe de taille validée, nommer ces classes (petite ville, moyenne, etc. par ex)
skim(etmun$population)
etmun <- etmun %>% 
  mutate(KPOP = case_when(population > 500 & population < 50000 ~ "1",
                          population > 50000 & population < 200000 ~ "2",
                          population > 200000 & population < 500000 ~ "3",
                          population > 500000 ~ "4",
                         TRUE ~ "NA")) %>% 
  group_by(Code_Network) %>% 
  mutate(Ncountry = length(unique(CountryName)))

table(etmun$KPOP)

## Count number of cities in each category pop by network
kTY_city <- etmun %>% 
  group_by(Code_Network, KPOP) %>% 
  summarise(N_Kcity_byNw = n())

## Add number of cities in each network
kTY_city <- left_join(network, kTY_city, by = "Code_Network")

## % 
kTY_city <- kTY_city %>% 
  mutate(P_Kcity_byNw = round(N_Kcity_byNw / Ncities * 100))

kTY_city_spread <- kTY_city %>% 
  select(Code_Network, KPOP, P_Kcity_byNw) %>% 
  spread(key = KPOP, value = P_Kcity_byNw, sep = "")

network <- left_join(network, kTY_city_spread)           

rm(kTY_city_spread, kTY_city)

network <- network %>% 
  mutate_at(vars("KPOP1", "KPOP2", "KPOP3", "KPOP4", "KPOPNA"), 
            replace_na, 0)


## le nb de pays concernés 
Ncountry <- etmun %>% 
  select(Code_Network, Ncountry) %>% 
  filter(!duplicated(Code_Network))

network <- network %>% left_join(Ncountry)

rm(Ncountry)

## l'area de la bonding box formée par le semis de point 
etmun <- etmun %>% filter_at(.vars = c("lng_GN", "lat_GN"), any_vars(!is.na(.)))
sfEtmun <- st_as_sf(etmun, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

vec <- unique(sfEtmun$Code_Network)
dfarea <- data.frame(Code_Network = character(59), area = double(59), row.names = vec)
for (i in vec) {
  bx <- sfEtmun %>% filter(Code_Network == i) %>% st_bbox()
  polyg <- st_as_sfc(st_bbox(bx))
  area <- st_area(polyg)
  dfarea[, 1] <- vec
  dfarea[i, 2] <- area
}

dfarea <- dfarea %>% 
  mutate(areaKM2 = area/1e+6)
rm(polyg, bx, area, i, vec)
network <- network %>% 
  left_join(., select(dfarea, Code_Network, areaKM2), by = "Code_Network")

rm(dfarea)



## centre de gravité : point moyen 
### Point moyen pondéré par la pop
coords3035 <- as.data.frame(st_coordinates(sfEtmun))
coords3035 <-  coords3035 %>% 
  mutate(Code_Network = sfEtmun$Code_Network,
         population = sfEtmun$population)
PG <- coords3035 %>% 
  group_by(Code_Network) %>% 
  summarise(X = mean(X), Y = mean(Y),
         Xp = weighted.mean(x = X, y = population),
         Yp = weighted.mean(x = Y, y = population)) %>% 
  mutate(CG = "point moyen")

network <- network %>% left_join(select(PG, Code_Network, X_mean = X, Y_mean = Y))

### Point médian marche pas
require(ICSNP)

vec <- unique(coords3035$Code_Network)
df <- data.frame(Code_Network = vec, 
                 medX = numeric(59), medY = numeric(59), row.names = vec)
for(i in unique(coords3035$Code_Network)){
  coord <- coords3035 %>% filter(Code_Network == i) %>% select(X, Y)
  df[i, 2] <-spatial.median(coord)[1]
  df[i, 3] <-spatial.median(coord)[2]
}

network <- network %>% left_join(select(df, Code_Network, X_med = medX, Y_med = medY))


# Clean envirmnt
rm(coord, df, i, vec, PG, coords3035)


# et la distance moyenne ou médiane. 
require(spdep)
##fonction dist moy au plus proche voisin (Ra)
MeanDistNN <- function(sf, k){
  
  sp <- as(sf, "Spatial")
  
  listNearNei <- knearneigh(sp@coords, k , longlat = F)
  
  NearNeigh <- knn2nb(listNearNei)
  
  distnei <- nbdists(NearNeigh, sp@coords, longlat = F)
  
  distnei <- unlist(distnei)
  
  meanDist <- mean(distnei)
  
  return(meanDist)
  
}

## compute for each network
NNdist <- sfEtmun %>%
  group_by(Code_Network) %>% 
  do(as.data.frame(MeanDistNN(sf=., k = 1))) %>% 
  rename(Ra = `MeanDistNN(sf = ., k = 1)` )

network <- network %>% left_join(NNdist)

## save for exploratR
write.csv2(network, "netETMUNforCAH.csv", row.names = FALSE, fileEncoding = "UTF-8")

# abstract df for PG
library(gridExtra)
library(grid)
d <- head(network[ , c(1, 3:7, 9:10, 15)])
pdf(file = "OUT/varCAH_etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
grid.table(d, rows = NULL)
dev.off()




## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

# HIERARCHICAL CLUSTERING ----
library(cluster)
library(ggdendro)
library(reshape2)

# compute classification ----
ComputeClassif <- function(df, varquanti, stand, method){
  classifObj <- agnes(x = df[, varquanti], diss = FALSE, metric = "euclidean", 
                      stand = stand, method = method)
  return(classifObj)
}

# plot dendrogram ----
PlotDendro <- function(classifobj){
  dendroPlot <- as.dendrogram(classifobj)
  dendroData <- dendro_data(dendroPlot, type = "rectangle")
  dendroGgplot <- ggplot(segment(dendroData)) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    scale_x_continuous("") + scale_y_continuous("") +
    theme_bw()
  
  return(dendroGgplot)
}

# plot inertia ----
PlotHeight <- function(classifobj){
  sortedHeight <- sort(classifobj$height, decreasing = TRUE)
  relHeigth <- sortedHeight / sum(sortedHeight) * 100
  tabHeight <- data.frame(NODES = factor(1:20),
                          INERTIE = relHeigth[1:20])
  
  heightPlot <- ggplot(tabHeight) +
    geom_bar(aes(x = NODES, y = INERTIE), fill = "grey30", stat = "identity") +
    scale_x_discrete("Nombre de classes") + scale_y_continuous("Niveau") +
    theme_bw()
  
  return(heightPlot)
}

# plot profile ----
PlotProfile <- function(classifobj, nbclus){
  dfOri <- as.data.frame(classifobj$data, stringsAsFactors = FALSE)
  clusId <- cutree(classifobj, k = nbclus)
  dfOri$CLUS <- factor(clusId,
                       levels = 1:nbclus,
                       labels = paste("CLASSE", 1:nbclus))
  clusProfile <- aggregate(dfOri[, 1:ncol(dfOri)-1],
                           by = list(dfOri$CLUS),
                           mean)
  colnames(clusProfile)[1] <- "CLASSE"
  clusLong <- melt(clusProfile, id.vars = "CLASSE")
  
  profilePlot <- ggplot(clusLong) +
    geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
    scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
    facet_wrap(~ CLASSE) + coord_flip() + theme_bw()
  
  return(list(PROFILE = profilePlot, CLUSID = dfOri$CLUS))
}


myVar <- c("Ncities", "Ncountry", "KPOP1", "KPOP2", "KPOP3", "KPOP4", "Ra", "areaKM2")
cah <- ComputeClassif(df = network %>% filter(Network_Name != "Covenant of Mayors" & 
                                                Network_Name != "Climate Alliance"),
                      varquanti = myVar, method = "ward", stand = TRUE)

cah <- ComputeClassif(df = network, varquanti = myVar, method = "ward", stand = TRUE)

dendro <- PlotDendro(classifobj = cah)
inert <- PlotHeight(classifobj = cah)
myProfiles <- PlotProfile(classifobj = cah, nbclus = 4)


network$cluster <- cutree(tree = cah, k = 4)
freq <- data.frame(table(networkb$cluster))

networkb <- network %>% filter(Network_Name != "Covenant of Mayors"& 
                                 Network_Name != "Climate Alliance")
networkb$cluster <- cutree(tree = cah, k = 4)
freq <- data.frame(table(networkb$cluster))

# pdf(file = "OUT/profilesCAH.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
# myProfiles
# dev.off()


