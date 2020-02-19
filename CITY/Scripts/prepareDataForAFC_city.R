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
# ACP
#==================================

# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/
library("FactoMineR")

df<- sfcity %>% 
  mutate_at(vars("POP2011_UMZ"), replace_na, 0) %>% 
  filter(continentCode == "EU") %>% 
  mutate(KPOPUMZ = as.factor(KPOP_UMZ),
         adminLevel = as.factor(adminLevel),
         TYPONUTS = as.factor(TYPO_NUTS),
         region = as.factor(region)) %>% 
  as.data.frame() %>% 
  select(-geometry)

#df[, "region"] <- as.factor(df[, "region"])
#sfcity <- sfcity %>% mutate_at(.vars = c(21:24), .funs = as.factor(.))

res.pca <- PCA(df[ , c(3:5, 20)],
               scale.unit = TRUE,
               #quanti.sup = ,
               quali.sup = df[ , 21:24],
               graph = FALSE)


#http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/80-acp-dans-r-avec-ade4-scripts-faciles/


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
