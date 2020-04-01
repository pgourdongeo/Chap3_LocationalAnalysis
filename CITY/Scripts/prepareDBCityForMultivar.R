
##==========================================================================##         
#                                 BD city - Analyse multivariée
#                          
# DESCRIPTION : préparation de la BD en vue d'analyses multivariées
#
#
# PG, AD, mars 2020
##==========================================================================##         

# Working directory huma-num
setwd("~/BD_Keep_Interreg/CITY")

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
sfCity <- st_as_sf(city, coords = c("lng_GN", "lat_GN"), crs = 4326) %>% 
  st_transform(crs = 3035)

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", 
               stringsAsFactors = FALSE, crs = 3035)
fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp", 
               stringsAsFactors = FALSE) %>% 
  st_transform(crs = 3035)

lau <- readRDS("Data/AdminDelimPop0611.RDS")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", 
                stringsAsFactors = FALSE, crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")


# ==== Spatial join with LAU2, UMZ and FUA ====

sfCity <- st_join(sfCity, 
                select(lau, CODE_LAU = Code_2, NAME_LAU = Name_2, PopAdmin06, PopAdmin11),
                join = st_intersects)

sfCity <- st_join(sfCity,
                  select(umz, ID_UMZ, NAME_UMZ = Name, POPUMZ11 = Pop2011),
                  join = st_intersects)

fua <- fua %>% filter(URAU_CATG == "L")
sfCity <- st_join(sfCity,
                  select(fua, ID_FUA = URAU_ID, NAME_FUA = URAU_NAME, POPFUA06 = URAU_POPL),
                  join = st_intersects)



# ==== Attribute join ====

## Simplified administration level
### Load csv
admin <- read_delim("../ETMUN/Script Analyse/admintyposimplifiee.csv", 
                    ";", escape_double = FALSE, trim_ws = TRUE)

admin[15,2] <- "seat of a first-order admin. division"
### Add admin level to the data
sfCity <- left_join(sfCity, admin, by = "fcodeName")

rm(admin)

## typo sciencePo
### load csv
typo <- read_delim("../CountryInfo_PoliticalTypo.csv", 
                   ";", escape_double = FALSE, trim_ws = TRUE)

### Add typo to sfCity
sfCity <- left_join(sfCity,
                    select(typo, countryCode = iso_a2, LocGovType_HorizontalPwrRelation,
                           LocGovType_VerticalPwrRelation, LocGovType_PoliticalLeadership,
                           LocGovTyp_EasternEurope, LocGovType_MunicipalAdmin, subregion,
                           MeanLAI_9014),
                    by = "countryCode")

rm(typo)


saveRDS(sfCity,"Data/DBCity_LauUmzFua.rds")
## ==== Spatial extent for ACP ====


## filter cities in Europe frame
#sfCityEur <- st_intersection(sfCity, rec)

# filter cities in UE 
iso <- c("IE", "GB", "PT", "ES", "FR", "BE", "NL", "LU", "DE", "DK",
         "AT", "IT", "GR", "SI", "HR", "CZ", "PL", "SK", "HU", "BG", 
         "RO", "EE", "LT", "LV", "CY", "FI", "SE", "MT")
sfCityEur <- sfCity %>% filter(countryCode %in% iso)



## ==== ACP ====
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/

library("FactoMineR")

df <- sfCityEur %>% st_drop_geometry()
df[ , 23:29] <- data.frame(apply(df[23:29], 2, as.factor))

df<- df %>% 
  mutate(label = paste(df$geonameId, df$asciiName, sep = "_")) %>% 
  mutate_at(vars("population","PopAdmin11", "PopAdmin11", "POPUMZ11", "POPFUA06"), 
            replace_na, 0) %>% 
  mutate(subregion = recode(subregion, "Western Asia"= "Southern Europe",  # recode Chypre & malta
                            "Middle East & North Africa" = "Southern Europe")) %>%  
  mutate(PopAdmin11 = as.numeric(ifelse(PopAdmin11 == 0, population, PopAdmin11)))

rownames(df) <- df[ , 31]



df2 <- df %>% select(c(3:5, 16,23))
skim(df2)
df2 <- df2 %>% filter(!is.na(adminLevel))
res.pca <- PCA(df2,
               scale.unit = TRUE,
               #ind.sup = df[ , 1],
               #quanti.sup = 6,
               quali.sup = 5,
               graph = FALSE)

library(explor)
explor(res.pca)

# library(GGally)
# ggpairs(df2, columns = c(1:6))
# 
# hist(df2$members_urbact)
# myVar <- df2 %>% 
#   filter(members_urbact>0)
# summary(myVar$members_urbact)


## ==== linear regression ====

reg <- lm(participations_eucicop ~ members_etmun , data = sfCityEur)
summary(reg)


### Equation de la droite de regression :
eq = paste0("y = ", round(reg$coefficients[1],2), " * x + ", round(reg$coefficients[2],2))

### Add line et equation
regEucEtm <- ggplot(sfCityEur, aes(x = members_etmun, y = participations_eucicop)) +
  geom_point () +
  theme_light() +
  labs(x = "Membres ETMUN", 
       y = "Nombre de participations aux projets Interreg") + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 20, y = 400, label = paste0(eq, "\nR2 = ", 
                                                              round(summary(reg)$r.squared, 2))) 


regEucEtm 

## Residuals
### add residuals and standart residuals to df
sfCityEur <- sfCityEur %>% 
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(sfCityEur$rezStand)

## residuals map (residuals standart)
rezMap_propChoro <- function(frame = rec, bgmap = sfEU, units, var, myVal, var2, 
                             title1, labFilter, source) {
  
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5)
  propSymbolsChoroLayer(units, 
                        var = var, inches = 0.3, border = "grey60", lwd = 0.5, symbols = "square",
                        var2 = var2, breaks = bks, col = cols,
                        legend.var.pos = NA, legend.var2.pos = NA)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, add = TRUE)
  
  # Add legend
  legendSquaresSymbols(pos = c(1000000, 4400000),
                       cex = 1,
                       var = myVal,
                       inches = 0.3, border = "grey60", lwd = 0.5, col = NA,
                       title.txt = title1, title.cex = 0.8, values.cex = 0.6)
  
  legendChoro(pos = c(1000000, 3000000), 
              cex = 0.9,
              title.txt = "Résidus Standardisés*", title.cex = 0.8, values.cex = 0.6,
              breaks = bks, col = cols, values.rnd = 2, nodata = F, 
              border = "grey60")
  
  # Add an explanation text
  labels <-  paste("*Résidus de la régression :\n", eq, 
                 ",\ndiscrétisés selon la moyenne\ndes résidus et 2 écarts-types", 
                 labFilter, sep = "")
  text(x = 1000000, y = 2300000, labels = labels, cex = 0.65, adj = 0)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = source,
        side = 1, 
        line = -1.2, 
        adj = 0.935,
        cex = 0.50)
  
  # # Add a layout
  # layoutLayer(title = "", 
  #             sources = source, 
  #             author = "PG, AD, 2019", 
  #             horiz = FALSE,
  #             col = NA, 
  #             frame = F, 
  #             scale = 500, 
  #             posscale = c(6500000, 1000000))
  
}

### defines a set of breaks and colors 
require(cartography)

skim(ol$PopAdmin11)

ol <- sfCityEur %>% filter(rezStand < -2*sdRez | rezStand > 2*sdRez | PopAdmin11 > 1000000)
bks <- c(min(ol$rezStand), -6 * sdRez, -4 * sdRez, -2 * sdRez, 
         2 * sdRez, 4 * sdRez, 6 * sdRez, max(ol$rezStand))
cols <- carto.pal("green.pal",3, "wine.pal",3, middle = TRUE)

skim(ol$PopAdmin11)

### Plot and save
pdf(file = "OUT/rez_eucicope_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = ol,
                 var = "PopAdmin11", 
                 myVal = c(10000, 500000, 3000000, 9000000),
                 var2 = "rezStand",
                 title1 = "Population administrative des villes en 2011", 
                 labFilter = "\n\nNB : Les villes comptant moins\nde 1 million d'habitants\nou sans résidus extrêmes\nn'apparaissent pas sur la carte", 
                 source = "Sources : EUCICOP 2019 ; ETMUN 2019 ; GEOSTAT LAU 2017, POPGRID 2011/ PG, AD, 2020")
dev.off()



mean(sfCityEur$rezStand)
sum(sfCityEur$rezStand)




library("factoextra")
indi <- get_pca_ind(res.pca)
indi <- as.data.frame(indi$coord)
. <- as.data.frame(indi$coord)
df <- cbind(df, .)






## ==== old ====



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



#### ---



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


sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")
sfcityE <- st_intersection(sfcity, rec)
library(ggplot2)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

ggplot()+
  geom_sf(data = sfEU, fill = "#bfbfbf", color = "white", size = 0.5) +
  geom_sf(data = sfcityE %>% filter(continentCode == "EU") ,
          mapping = aes(colour = members_etmun_K), size = 0.75, shape = 20, show.legend = NA)+
  scale_colour_manual(values = c("orange", "yellow", "grey60", "red", "#375D81")) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(st_bbox(rec)[3]-800000), y = c(st_bbox(rec)[2]+280000)) +
  geom_sf(data = rec, fill = NA, color = "ivory4", size = 0.5) +
  coord_sf(crs = 3035, datum = NA,
           xlim = st_bbox(rec)[c(1,3)],
           ylim = st_bbox(rec)[c(2,4)]) +
  theme_void() +
  theme(legend.position =  c(0.18, 0.60), 
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5))

#===
# Corrélation
#===
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



#===
# ACP  it works !
#===

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


#===
# ===K-means====
#===

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

#===
# CAH
#===

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






#===
# MFA
#===
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

#===
# ACM
#===

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





#### ----- Multiple regression ------

library(performance)

mEucicop<-lm(participations_eucicop ~ PopAdmin11 + MeanLAI_9014, data = df)

summary(mEucicop)
check_model(mEucicop)

model <- lm(mpg ~ wt * cyl + gear, data = mtcars)
str(mtcars)
