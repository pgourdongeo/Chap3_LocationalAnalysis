vu###############################################################################
#                                 BD city - Analyse multivariée
#                          
# DESCRIPTION : à partir de la BD city, création des variables en vue d' 
#               analyses multivariées (ACP/ACM)
#
#
# PG, AD, février 2020
##############################################################################

# Working directory huma-num
#setwd("~/BD_Keep_Interreg/CITY")

setwd("~/git/Chap3_LocationalAnalysis/CITY")
options(scipen = 999)


# library
library(skimr)
library(tidylog)
library(tidyverse)
library(readr)
library(sf)
library(mapview)


# load data
city <- readRDS("Data/DBCity.rds")



# Prepare data

## 1. discretisation of number members/participations/partners by city
## 5 classes : participation nulle ; unique ; multiple ; forte ; très forte

require(cartography)

### etmun
myVar <- city %>% filter(members_etmun > 1) 
myVar <- as.numeric(myVar$members_etmun)
bks <- c(getBreaks(v = myVar, method = "equal", nclass = 3))

city <- city %>% 
  mutate(members_etmun_K = case_when(members_etmun == 1 ~ "unique",
                                     members_etmun >= bks[1] & members_etmun < bks[2] ~ "multiple",
                                     members_etmun >= bks[2] & members_etmun < bks[3] ~ "forte",
                                     members_etmun >= bks[3] & members_etmun <= bks[4] ~ "très forte",
                                     members_etmun == 0 ~ "nulle"))

freq <- as.data.frame(table(city$members_etmun_K))

### urbact
myVar <- city %>% filter(members_urbact > 1) 
myVar <- as.numeric(myVar$members_urbact)
bks <- c(getBreaks(v = myVar, method = "equal", nclass = 3))

city <- city %>% 
  mutate(members_urbact_K = case_when(members_urbact == 1 ~ "unique",
                                      members_urbact >= bks[1] & members_urbact < bks[2] ~ "multiple",
                                      members_urbact >= bks[2] & members_urbact < bks[3] ~ "forte",
                                      members_urbact >= bks[3] & members_urbact <= bks[4] ~ "très forte",
                                      members_urbact == 0 ~ "nulle"))

freq <- as.data.frame(table(city$members_urbact_K))

### eucicop - participation
myVar <- city %>% filter(participations_eucicop > 1) 
myVar <- as.numeric(myVar$participations_eucicop)
#bks <- c(getBreaks(v = myVar, method = "equal", nclass = 3))
bks <- c(2, 50, 200, 700)

city <- city %>% 
  mutate(participations_eucicop_K = case_when(participations_eucicop == 1 ~ "unique",
                                      participations_eucicop >= bks[1] & participations_eucicop < bks[2] ~ "multiple",
                                      participations_eucicop >= bks[2] & participations_eucicop < bks[3] ~ "forte",
                                      participations_eucicop >= bks[3] & participations_eucicop <= bks[4] ~ "très forte",
                                      participations_eucicop == 0 ~ "nulle"))

freq <- as.data.frame(table(city$participations_eucicop_K))


### eucicop - partner
myVar <- city %>% filter(partners_eucicop > 1) 
myVar <- as.numeric(myVar$partners_eucicop)
bks <- c(2, 50, 200, 700)

city <- city %>% 
  mutate(partners_eucicop_K = case_when(partners_eucicop == 1 ~ "unique",
                                        partners_eucicop >= bks[1] & partners_eucicop < bks[2] ~ "multiple",
                                        partners_eucicop >= bks[2] & partners_eucicop < bks[3] ~ "forte",
                                        partners_eucicop >= bks[3] & partners_eucicop <= bks[4] ~ "très forte",
                                        partners_eucicop == 0 ~ "nulle"))

freq <- as.data.frame(table(city$partners_eucicop_K))

rm(freq, myVar, bks)




## 2. City size (population)

### population GN
city$population <- as.numeric(city$population)
skim(city$population)
city <- city %>% 
  mutate(KPOP_GN = case_when(population > 0 & population < 50000 ~ "petite ville",
                          population >= 50000 & population < 150000 ~ "ville moyenne",
                          population >= 150000 & population < 300000 ~ "grande ville",
                          population >= 300000 ~ "très grande ville"))

### population UMZ
#### Load UMZ
umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)

#### transform df city to sf
sfcity <- st_as_sf(city, coords = c("lng_GN", "lat_GN"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

#### join 'pop2011' UMZ to city
sfcity <- st_join(sfcity, select(umz, ID_UMZ, NAME_UMZ = Name, POP2011_UMZ = Pop2011),
                  join = st_intersects)

skim(sfcity$POP2011_UMZ)

#mapview(umz) + mapview(sfcity)

#### classification
sfcity <- sfcity %>% 
  mutate(KPOP_UMZ = case_when(POP2011_UMZ > 0 & POP2011_UMZ < 50000 ~ "petite ville",
                              POP2011_UMZ >= 50000 & POP2011_UMZ < 150000 ~ "ville moyenne",
                              POP2011_UMZ >= 150000 & POP2011_UMZ < 300000 ~ "grande ville",
                              POP2011_UMZ >= 300000 ~ "très grande ville"))


rm(umz)




## 3. Simplified administration level
### Load csv
admin <- read_delim("../ETMUN/Script Analyse/admintyposimplifiee.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

### Add admin level to the data
sfcity <- left_join(sfcity, admin, by = "fcodeName")

rm(admin)




## 4. NUTS type (urban/rural)
### Load nuts
nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035)

#mapview(nutsUR) + mapview(sfcity)


### FIRST - snap outsiders  !!! ----

#### join city points to nuts to have outsiders
iso <- c("SE", "FI", "EE", "EU", "LT", "PL", "LI", "NL", "HU", "ES",
         "FR", "GR", "IE", "IT", "AT", "BE", "BG", "CY", "CZ", "DE",
         "DK", "LU", "LV", "PT", "RO", "SI", "SK", "SM", "GB")

outsiders <- sfcity %>% 
  filter(continentCode == "EU") %>% 
  filter(countryCode %in% iso) %>% 
  st_join(., select(nutsUR, Nuts_Id)) %>% 
  filter(is.na(Nuts_Id))

#mapview(nutsUR) + mapview(outsiders)

#### function to snap outsiders points (due to generalisation of country polygons) to the nearest country polygon
#### Source : https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf

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

#### Apply function
require(purrr) # correction of the nuts shape topology
snap_outsiders <- st_snap_points(outsiders, compact(nutsUR$geometry), max_dist = 50000)

#### replace coords to outsiders 
outsiders$geometry <- snap_outsiders
outsiders <- outsiders %>%  select(-Nuts_Id)
#mapview(nutsUR) + mapview(outsiders)


#### join ousiders snaped to insiders
id <- outsiders$geonameId
sfcity <- sfcity %>% 
  filter(!geonameId %in% id) %>% 
  rbind(., outsiders)

#### verif
# outsiders <- sfcity %>% 
#   filter(continentCode == "EU") %>% 
#   filter(countryCode %in% iso) %>% 
#   st_join(., select(nutsUR, Nuts_Id)) %>% 
#   filter(is.na(Nuts_Id))

#mapview(nutsUR) + mapview(outsiders)

rm(id, snap_outsiders, outsiders, st_snap_points, iso)



#### ------------



### Recode variable Typo7
nutsUR <- nutsUR %>% 
  mutate(Typo7_v2 = recode(Typo_7Clv2,
                           "4" = "Régions sous dominance\nd'une métropole",         
                           "6" = "Régions avec densité\nurbaine élevée",            
                           "5" = "Régions à majorité\nde villes moyennes",         
                           "7" = "Régions avec densité\nurbaine et rurale élevées",   
                           "1" = "Régions rurales\nsous influence métropolitaine",
                           "2" = "Régions rurales\navec villes petites et moyennes",
                           "3" = "Régions rurales isolées"))


### add typology to the sf with snaped points
sfcity <- st_join(sfcity, select(nutsUR, TYPO_NUTS = Typo7_v2), join = st_intersects)

#### pts in a cross borders have 2 typo nuts -- to remove 
doublon <- sfcity %>% filter(duplicated(geonameId))
doublon <- doublon$geonameId
doublon <- sfcity %>% filter(geonameId %in% doublon)
#mapview(nutsUR) + mapview(doublon)

#### remove doublon
sfcity <- sfcity %>% filter(!duplicated(geonameId))

rm(doublon, nutsUR)




## 5. European region 
library(countrycode)

sfcity$region <- countrycode(sfcity$countryCode, origin ="iso2c", destination ="region")

### Warning message: Some values were not matched unambiguously: XK

sfcity <-  sfcity %>% 
  mutate_at(vars("region"), replace_na, "Southern Europe")




#==================================
# Corrélation
#==================================
library(GGally)
cityLog <- sfcity %>% 
  filter(continentCode == "EU") %>% 
  mutate_at(vars("POP2011_UMZ"), replace_na, 0) %>% 
  select(members_etmun, members_urbact, participations_eucicop, POP2011_UMZ) %>% 
  transmute(members_etmun, members_urbact, participations_eucicop, POP2011_UMZ,
         etmunLog = log10(members_etmun),
         urbactlog = log10(members_urbact),
         eucicoplog = log10(participations_eucicop),
         POPLog = log10(POP2011_UMZ)) %>% 
  as.data.frame() %>% 
  select(-geometry)

ggpairs(cityLog[ , 5:8])



#==================================
# ACP  it works !
#==================================

# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/
library("FactoMineR")

df<- sfcity %>% 
  mutate(label = paste(sfcity$geonameId, sfcity$asciiName, sep = "_")) %>% 
  mutate_at(vars("POP2011_UMZ"), replace_na, 0) %>% 
  filter(continentCode == "EU") %>% 
  mutate(region = recode(region, "Western Asia"= "Southern Europe")) %>% # recode Chypre
  mutate(KPOPUMZ = as.factor(KPOP_UMZ),
         adminLevel = as.factor(adminLevel),
         region = as.factor(region),
         TYPONUTS = as.factor(TYPO_NUTS)) %>% 
  as.data.frame() %>% 
  select(-geometry)  

rownames(df) <- df[ , 25]
#df <- select(df, -geonameId, -asciiName)

#df[, "region"] <- as.factor(df[, "region"])
#sfcity <- sfcity %>% mutate_at(.vars = c(21:24), .funs = as.factor(.))

#res.pca <- PCA(df[ , c(3:5, 12, 20)], scale.unit = TRUE, graph = FALSE)


df2 <- df %>% select(c(3:5, 12, 22))
res.pca <- PCA(df2,
               scale.unit = TRUE,
               #ind.sup = df[ , 1],
               #quanti.sup = ,
               quali.sup = 5,
               graph = FALSE)

#library(explor)
explor(res.pca)




library("factoextra")
indi <- get_pca_ind(res.pca)
indi <- as.data.frame(indi$coord)
. <- as.data.frame(indi$coord)
df <- cbind(df, .)



#http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/80-acp-dans-r-avec-ade4-scripts-faciles/


#==================================
# K-means
#==================================

# K-means vectors population

## Compute explained variance (setting K) on AFC results

var.expl <- c()
for (i in 1:40){
  result <- kmeans(x=indi,
                   centers=i,
                   iter.max=100,
                   nstart=5)
  var.expl <- append(var.expl,result$betweenss/result$totss)
}

var.expl <- data.frame(n=1:164,
                       var=var.expl)


ggplot(var.expl, aes(x = n, y = var))+
  geom_point() + scale_x_continuous(breaks = seq(0,60,5)) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        plot.title = element_text(colour = "brown")) +
  labs(x = "k", y = "variance explained") +
  labs(title = "K-Means Clustering on Population Vectors ")

# Performing simple K-mean Partition
## Dendrogram
K=6
clusters <- kmeans(x=indi,
                   centers=K,
                   iter.max=100,
                   nstart=5)
clusters$size/sum(clusters$size)
clusters$centers
#apply(clusters$withinss, 1, sum)

clusters$size


clustersCenters <- as.data.frame(clusters$centers)
clustersCenters <- clustersCenters %>%
  mutate(Cluster = row.names(.))%>%
  mutate(n = clusters$size)

#library(reshape2)
clusLong <- clustersCenters %>% 
  select(-n) %>% 
  melt(., id.vars = "Cluster") 
  
profilePlot <- ggplot(clusLong) +
  geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
  scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
  facet_wrap(~ Cluster) + coord_flip() + theme_bw()
profilePlot

#==================================
# CAH
#==================================

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


myVar <- c("Dim.1", "Dim.2", "Dim.3", "Dim.4")
cah <- ComputeClassif(df = df,
                      varquanti = myVar, method = "ward", stand = FALSE)


dendro <- PlotDendro(classifobj = cah)
inert <- PlotHeight(classifobj = cah)
myProfiles <- PlotProfile(classifobj = cah, nbclus = 4)


df$cluster <- cutree(tree = cah, k = 4)
freq <- data.frame(table(networkb$cluster))


# pdf(file = "OUT/profilesCAH.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
# myProfiles
# dev.off()






#==================================
# MFA
#==================================
#♠ http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/77-afm-analyse-factorielle-multiple-avec-r-l-essentiel/
df3 <- df %>% 
  select(c(3:5, 12, 20, 22, 24, 26))

res.mfa <- MFA(df3,
               group = c(3, 2, 1, 2),
               type = c(rep("s", 2), rep("n", 2)),
               ncp = 4,
               name.group = c("coop", "pop", "admin", "geo"),
               #num.group.sup = c(4),
               graph = FALSE)



#library("factoextra")
eig.val <- get_eigenvalue(res.mfa)
eig.val
fviz_screeplot(res.mfa)
group <- get_mfa_var(res.mfa, "group")
# Coordonnées des groupes
group$coord
# Cos2: qualité de représentation des groupes
group$cos2
# Contributions des dimensions
group$contrib

fviz_mfa_var(res.mfa, "group")

quanti.var <- get_mfa_var(res.mfa, "quanti.var")
quanti.var$contrib
quanti.var$coord

#==================================
# ACM
#==================================

## clean df to ACM
city_afc <- sfcity %>% 
  #filter(continentCode == "EU") %>% 
  select(geonameId, members_etmun_K, members_urbact_K, participations_eucicop_K, partners_eucicop_K,
         KPOP_GN, KPOP_UMZ, adminLevel, TYPO_NUTS, region) %>% 
  as.data.frame() %>% 
  select(-geometry)

# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/84-acm-dans-r-avec-factominer-scripts-faciles-et-cours/
library(FactoMineR)

df<- Map(paste, city_afc, names(city_afc), sep = '_') %>% 
   as.data.frame() %>% 
   select(-geonameId)
 
res.mca <- MCA(df, graph=FALSE)
eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")


plot(res.mca, 
     invisible = "ind",
     cex = 0.8,
     autoLab = "yes")

df2 <- df %>% select(members_etmun_K, KPOP_UMZ, adminLevel)

res.mca <- MCA(df2, graph=FALSE)
eig.val <- res.mca$eig
barplot(eig.val[, 2], 
        names.arg = 1:nrow(eig.val), 
        main = "Variances Explained by Dimensions (%)",
        xlab = "Principal Dimensions",
        ylab = "Percentage of variances",
        col ="steelblue")


plot(res.mca, 
     invisible = "ind",
     cex = 0.8)

library(explor)
explor(res.mca)
