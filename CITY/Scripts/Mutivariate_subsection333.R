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

library(tidyverse)
library(tidylog)
library(skimr)
library(readr)
library(sf)
library(mapview)
library(FactoMineR)
library(explor)
library(GGally)

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

# mapview(lau%>% filter(str_detect(Code_2, "HU") ))
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
unique(admin$adminLevel)
admin <- admin %>% mutate(adminLevel = recode(adminLevel, "seat of other admin. division" = "populated place"))
### Add admin level to the data
sfCity <- left_join(sfCity, admin, by = "fcodeName")

rm(admin)

## typo sciencePo
### load csv
typo <-read.csv2("../CountryInfo_PoliticalTypo.csv",  stringsAsFactors = F)

### Add typo to sfCity
sfCity <- left_join(sfCity,
                    select(typo, countryCode = iso_a2, LocGovType_HorizontalPwrRelation,
                           LocGovType_VerticalPwrRelation, LocGovType_PoliticalLeadership,
                           LocGovTyp_EasternEurope, LocGovType_MunicipalAdmin, subregion,
                           MeanLAI_9014),
                    by = "countryCode")

rm(typo)


# saveRDS(sfCity,"Data/DBCity_LauUmzFua.rds")
## ==== Spatial extent for ACP ====




# filter cities in UE 
iso <- c("IE", "GB", "PT", "ES", "FR", "BE", "NL", "LU", "DE", "DK",
         "AT", "IT", "GR", "SI", "HR", "CZ", "PL", "SK", "HU", "BG", 
         "RO", "EE", "LT", "LV", "CY", "FI", "SE", "MT")
sfCityEur <- sfCity %>% filter(countryCode %in% iso)


## Prepare a tibble for analysis

df <- sfCityEur %>% st_drop_geometry()
df[ , 23:29] <- data.frame(apply(df[23:29], 2, as.factor))

df<- df %>% 
  mutate(label = paste(df$asciiName, df$countryCode, df$geonameId, sep = ", ")) %>% 
  mutate_at(vars("population","PopAdmin11", "PopAdmin11", "POPUMZ11", "POPFUA06"), 
            replace_na, 0) %>% 
  mutate(subregion = recode(subregion, "Western Asia"= "Southern Europe",  # recode Chypre & malta
                            "Middle East & North Africa" = "Southern Europe")) %>%  
  mutate(PopAdmin11 = as.numeric(ifelse(PopAdmin11 == 0, population, PopAdmin11)))%>%
  mutate(adminLevel = replace_na(adminLevel, "other"))


# DBcityBeforeCorr<- readRDS("Data/DBCity_beforeCorr.rds")
# nrow(DBcityBeforeCorr)-nrow(city)
# 
1- nrow(df)/nrow(city)
# skim(df)
## ====== Filter the dataframe ======
# After exploration of DBcity without filter, it's obvious that all the zero(s) and very low value impact all analysis

# First filter remove city with at least 2  participations (all kinds : eucicop/etmun)
#and with admin pop lower than 1000

#Tot Participation

df <- df %>% mutate(TotParticipAllKind = rowSums(.[3:5]))

## Histogramme pour la thèse

DfFr <- df %>% rename("Nombre d'adhésions\naux associations ETMUN" = members_etmun ,
                      "Nombre de participations\nau programme URBACT" = members_urbact ,
                      "Nombre de participations\nEUCICOP" = participations_eucicop,
                      "Population 2011 (LAU)" = PopAdmin11 )


DfFr %>% select(c(3:5, 16)) %>% 
  gather() %>% 
  ggplot(aes(value)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()+
  labs(x = "Valeurs", 
       y = "Nombres d'entités GN (log10)",
       caption = "Sources : EUCICOP 2019 ; ETMUN, 2019 / PG. 2020")
ggsave("OUT/HistDBcity_EU.pdf", width = 8.3, height = 5.8, units = "in")

# Try ranking variables
df <- df %>% arrange(desc(TotParticipAllKind)) %>%  
  mutate_at(.vars = c("members_etmun", "members_urbact", 
                      "participations_eucicop","PopAdmin11"), 
            list( rank = ~ rank(desc(.), ties.method = 'first'))) 

# Rank Pop national perimeter

df <- df %>% group_by(countryCode) %>% 
  mutate(RankPop2011_Nat = row_number(desc(PopAdmin11)))%>% 
  ungroup()



## Ratio participations per hab

df <- df %>%mutate_at(.vars = vars(3:5), 
                      list(per10kInh = ~./PopAdmin11*10000)) 


# rank nat variable ratio and stock participation

df <- df %>% group_by(countryCode) %>% 
  mutate_at(vars(3:5,37:39), list(RankNat = ~row_number(desc(.)))) %>% 
  ungroup()

### ===== Exploration of Etmun and Eucicop participation by size class =====

 #==== Discrétisation des variables ====
library(cartography)

### etmun

myVar <- as.numeric(df$members_etmun)
skim(myVar)
hist(myVar)
summary(df %>% filter(members_etmun>1)%>% select(members_etmun))

df <- df %>% 
  mutate(members_etmun_K = case_when(members_etmun == 1 ~ "b) une adhésion",
                                     members_etmun >= 2 & members_etmun <=5  ~ "c) entre 2 et 5",
                                     members_etmun > 5 & members_etmun <= 15  ~ "d) entre 6 et 15",
                                     members_etmun > 15 & members_etmun <= 43  ~ "e) supérieur à 16",
                                     members_etmun == 0 ~ "a) nulle"))

freq <- as.data.frame(table(df$members_etmun_K))


### eucicop - participation

myVar <- as.numeric(df$participations_eucicop)
hist(myVar)
bks <- c(getBreaks(v = myVar, method = "geom", nclass = 8))

summary(df %>% filter(participations_eucicop>1)%>% select(participations_eucicop))


df <- df %>% 
  mutate(participations_eucicop_K = case_when(participations_eucicop == 1 ~ "b) une participation",
                                              participations_eucicop > 1 & participations_eucicop <= 4 ~ "c) entre 2 et 4",
                                              participations_eucicop > 4 & participations_eucicop <= 10 ~ "d) entre 5 et 10",
                                              participations_eucicop > 10 & participations_eucicop <= 50 ~ "e) entre 10 et 50",
                                              participations_eucicop > 50 & participations_eucicop <= 701 ~ "f) supérieur à 50",
                                              participations_eucicop == 0 ~ "a) nulle"))

freq <- as.data.frame(table(df$participations_eucicop_K))

rm(freq, myVar, bks)


###  population admin 2011

myVar <- as.numeric(df$PopAdmin11)
hist(myVar)
bks <- c(getBreaks(v = myVar, method = "geom", nclass = 8))

summary(df$PopAdmin11)


df <- df %>% 
  mutate(popadmin11_K = case_when(PopAdmin11 <= 1000 ~ "a) inférieure à 1000",
                                              PopAdmin11 > 1000 & PopAdmin11 <= 5000 ~ "b) inférieure à 5000",
                                              PopAdmin11 > 5000 & PopAdmin11 <= 20000 ~ "c) inférieure à 20 000",
                                              PopAdmin11 > 20000 & PopAdmin11 <= 50000 ~ "d) inférieure à 50 000",
                                              PopAdmin11 > 50000 & PopAdmin11 <= 100000 ~ "e) inférieure à 100 000",
                                              PopAdmin11 > 100000 & PopAdmin11 <= 250000 ~ "f) inférieure à 250 000",
                                              PopAdmin11 > 250000 & PopAdmin11 <= 500000 ~ "g) inférieure à 500 000",
                                              PopAdmin11 > 500000 & PopAdmin11 <= 1000000 ~ "h) inférieure à 1 000 000",
                                              PopAdmin11 > 1000000 ~ "i) supérieure à 1 000 000"
                                              ))

freq <- as.data.frame(table(df$popadmin11_K))


#Admin level in ordinal value
unique(df$adminLevel)

df <- df %>% 
  mutate(adminlevel_order = case_when(adminLevel == "capital of a political entity" ~ 1,
                                      adminLevel == "seat of a first-order admin. division" ~ 2,
                                      adminLevel == "seat of a second-order admin. division" ~ 3,
                                      adminLevel == "populated place" ~ 4,
                                      adminLevel == "other" ~ 5))
                                  

## Urbact Bolean
df <- df %>% 
  mutate(Urbact_YN = case_when(members_urbact > 0 ~ "URBACT City",
                               members_urbact == 0 ~ "Non-URBACT City"))



###  ratio etmun

myVar <- as.numeric(df$members_etmun_per10kInh)
hist(myVar)
bks <- c(getBreaks(v = myVar, method = "geom", nclass = 6))

summary(df %>% filter(is.finite(members_etmun_per10kInh))%>% filter(members_etmun_per10kInh> 10) %>% select(members_etmun_per10kInh))


df <- df %>% 
  mutate(etmunper10kInh_K = case_when(members_etmun_per10kInh > 0 & members_etmun_per10kInh <= 1 ~ "b) entre 0 et 1",
                                              members_etmun_per10kInh > 1 & members_etmun_per10kInh <= 4 ~ "c) entre 1 et 4",
                                              members_etmun_per10kInh > 4 & members_etmun_per10kInh <= 10 ~ "d) entre 4 et 10",
                                              members_etmun_per10kInh > 10 & members_etmun_per10kInh <= 30 ~ "e) entre 10 et 30",
                                              members_etmun_per10kInh > 30 & members_etmun_per10kInh <= 100 ~ "f) entre 30 et 100",
                                              members_etmun_per10kInh > 100 ~ "g) supérieur à 100",
                                              members_etmun_per10kInh == 0 ~ "a) nulle"))

freq <- as.data.frame(table(df$etmunper10kInh_K))



###  ratio eucicop

myVar <- as.numeric(df$participations_eucicop_per10kInh)
hist(myVar)
bks <- c(getBreaks(v = myVar, method = "geom", nclass = 6))

summary(df %>% filter(is.finite(participations_eucicop_per10kInh))%>% 
          filter(participations_eucicop_per10kInh> 10) %>% 
          select(participations_eucicop_per10kInh))


df <- df %>% 
  mutate(eucicopper10kInh_K = case_when(participations_eucicop_per10kInh > 0 & participations_eucicop_per10kInh <= 1 ~ "b) entre 0 et 1",
                                        participations_eucicop_per10kInh > 1 & participations_eucicop_per10kInh <= 5 ~ "c) entre 1 et 5",
                                        participations_eucicop_per10kInh > 5 & participations_eucicop_per10kInh <= 10 ~ "d) entre 5 et 10",
                                        participations_eucicop_per10kInh > 10 & participations_eucicop_per10kInh <= 40 ~ "e) entre 10 et 40",
                                        participations_eucicop_per10kInh > 40 & participations_eucicop_per10kInh <= 100 ~ "f) entre 40 et 100",
                                        participations_eucicop_per10kInh > 100 ~ "g) supérieur à 100",
                                        participations_eucicop_per10kInh == 0 ~ "a) nulle"))

freq <- as.data.frame(table(df$eucicopper10kInh_K))


### Graphic exploration 
library(cartography)
display.carto.pal("turquoise.pal")
pal <- carto.pal("turquoise.pal", n1 = 5)
#ETMUN

tableEtmun <- table(df$popadmin11_K,df$members_etmun_K)
tableEtmunRows <- as.data.frame(prop.table(tableEtmun,1))
tableEtmunRows <- tableEtmunRows %>% mutate(Freq = Freq * 100)

tableEtmunCol <- as.data.frame(prop.table(tableEtmun,2))
tableEtmunCol <- tableEtmunCol %>% mutate(Freq = Freq * 100)

ggplot(tableEtmunRows, aes(Var2, Var1, fill= Freq)) + 
  geom_tile() +
  scale_fill_gradient(low="grey90", high=pal[4]) +
  geom_text(aes(label = paste(round(Freq, 2),"%", sep = "" )))+
  labs(y= "Classes de taille (Population Admin 2011)",
       x = "Nombre d'adhésions à des associations (ETMUN)", 
       fill = "Fréquence relative\npar classe\nde population (%)")+
  theme(axis.text.x = element_text(angle = 25, vjust = 0.9))

ggsave("OUT/ETMUNAdh_perCitySizeClasse.pdf", width = 8.3, height = 5.8, units = "in")

ggplot(tableEtmunCol, aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  scale_fill_gradient(low="grey90", high=pal[4]) +
  geom_text(aes(label = paste(round(Freq, 2),"%", sep = "" )))+
  labs(x= "Classes de taille (Population Admin 2011)",
       y = "Nombre d'adhésions à des associations (ETMUN)", 
       fill = "Fréquence relative\npar nombre\nd'adhésions (%)")+
  theme(axis.text.x = element_text(angle = 25, vjust = 0.9))
ggsave("OUT/CitySize_perETMUNadh.pdf", width = 8.3, height = 5.8, units = "in")
# EUCICOP
tableEucicop <- table(df$popadmin11_K,df$participations_eucicop_K)

tableEucicopRows <- as.data.frame(prop.table(tableEucicop, 1))
tableEucicopRows <- tableEucicopRows %>% mutate(Freq = Freq * 100)

tableEucicopCol <- as.data.frame(prop.table(tableEucicop,2))
tableEucicopCol <- tableEucicopCol %>% mutate(Freq = Freq * 100)

ggplot(tableEucicopRows, aes(Var2, Var1, fill = Freq)) + 
  geom_tile() +
  scale_fill_gradient(low="grey90", high=pal[4]) +
  geom_text(aes(label = paste(round(Freq, 2),"%", sep = "" )))+
  labs(y= "Classes de taille (Population Admin 2011)",
       x = "Nombre de participations (EUCICOP)", 
       fill =  "Fréquence relative\npar classe\nde population (%)")+
  theme(axis.text.x = element_text(angle = 25, vjust = 0.9))

ggsave("OUT/EUCICOPpart_perCitySizeClasse.pdf", width = 8.3, height = 5.8, units = "in")

ggplot(tableEucicopCol , aes(Var1, Var2, fill= Freq)) + 
  geom_tile() +
  scale_fill_gradient(low="grey90", high=pal[4]) +
  geom_text(aes(label = paste(round(Freq, 2),"%", sep = "" )))+
  labs(x= "Classes de taille (Population Admin 2011)",
       y = "Nombre de participations (EUCICOP)", 
       fill = "Fréquence relative\npar nombre de\nparticipations (%)")+
  theme(axis.text.x = element_text(angle = 25, vjust = 0.9))

ggsave("OUT/CitySize_perEUCICOPpart.pdf", width = 8.3, height = 5.8, units = "in")
## First Filter by total participation (melt all kind)
# df2 <- df %>% tidylog::filter(TotParticipAllKind >2) # remove 62% of the cities

#Admin Pop
skim(df$PopAdmin11)
df2 <- df %>% tidylog::filter(PopAdmin11> 5000)# 453 rows removed

nrow(df2)/nrow(df)*100

# description du filtre pop sur le nombre de zéros

N0df <- df %>% filter_at(vars(3,5), any_vars(.== 0)) ## 17 % non zero Etmun and Eucicop

NOdf2 <- df2 %>% filter_at(vars(3,5), any_vars(.== 0)) ## 29% non zero Etmun and EUcicop

str(df2)
# Tranfo en log
dummy = 1
dfLogFilter <- df %>%  mutate_at(vars(37:39,32), ~.+dummy) %>% mutate_at(vars(37:39,32), list(log10 = ~log10(.)))
dfLogFilter <- dfLogFilter %>% filter(PopAdmin11> 5000)
##Change agregate level ==> fua

FuaParticip <- df %>% select(members_etmun, members_urbact, 
                             participations_eucicop, countryCode, 
                             ID_FUA, NAME_FUA, POPFUA06, MeanLAI_9014, starts_with("LocGov")) %>% 
                      group_by(ID_FUA) %>% 
                      mutate_at(vars(1:3), list(FUA = ~sum(.)))%>%
                      select(-members_etmun,-members_urbact,-participations_eucicop)%>%
                      distinct()%>%
                      filter(!is.na(ID_FUA))

FuaParticip <- FuaParticip %>%mutate_at(.vars = vars(11:13), 
                      list(per10kInh = ~./POPFUA06*10000)) 

FuaParticip %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

ggpairs(FuaParticip, columns = c(11:13,4,14:16), 
        lower = list(continuous = wrap("points", alpha = 0.2)))
#write.csv2(dfLogFilter, "Data/DbCityLogFiltered_exploratR.csv", row.names = F)
## ==== Uni and bivariates exploration ====
# filter Paris an london

df3 <- df2 %>% filter_at(.vars = vars(16,19), all_vars(.<9000000))

# Hist continuous variables
df %>% select(c(3:5, 37:39)) %>% 
  gather() %>% 
  ggplot(aes(value)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

df3 %>% keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

df3 %>% select(c(3:5, 16, 19, 22)) %>% 
  gather() %>% 
  ggplot(aes(value)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

dfLogFilter%>% select(c(3:5, 16, 19, 22)) %>% 
  gather(value = "valueLog10") %>% 
  ggplot(aes(valueLog10)) + scale_y_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

df2FR %>% select(c(16, 37:39)) %>% 
  gather(value = "valueLog10") %>% 
  ggplot(aes(valueLog10)) + scale_y_continuous(trans = "log10")+
  scale_x_continuous(trans = "log10")+
  facet_wrap(~ key, scales = "free") +
  geom_histogram()
### GGpairs pour la thèse (filtre 5000 hab uniquement)
library(GGally)

df3FR <- df2  %>% filter(PopAdmin11<9000000)%>%
  rename("NbAdh\nETMUN" = members_etmun ,
         "NbPart\nURBACT" = members_urbact ,
         "NbPart\nEUCICOP" = participations_eucicop,
         "Pop2011" = PopAdmin11,
         "ETMUN\n10kHab" = members_etmun_per10kInh,
         "URBACT\n10kHab" = members_urbact_per10kInh,
         "EUCICOP\n10kHab" = participations_eucicop_per10kInh)

ggpairs(df2FR, columns = c(3:5,16,37:39), 
        lower = list(continuous = wrap("points", alpha = 0.2)))

ggsave("OUT/GGallyDBCity_EU_5K.png", width = 8.3, height = 5.8, units = "in")
# bivariate exploration

library(GGally)
ggpairs(df, columns = c(36,3,5), 
        lower = list(continuous = wrap("points", alpha = 0.2)))

ggpairs(df3, columns = c(3:5, 16), 
        lower = list(continuous = wrap("points", alpha = 0.1)))

ggpairs(dfLogFilter, columns = c(36, 40:42), 
        lower = list(continuous = wrap("points", alpha = 0.1)))

ggpairs(df3, columns = c(36:39), 
        lower = list(continuous = wrap("points", alpha = 0.1)))
## ==== ACP ====
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/73-acp-analyse-en-composantes-principales-avec-r-l-essentiel/


df3FR <- df2  %>% 
  rename("NbAdh\nETMUN" = members_etmun ,
         "NbPart\nURBACT" = members_urbact ,
         "NbPart\nEUCICOP" = participations_eucicop,
         "Pop2011" = PopAdmin11,
         "ETMUN\n10kHab" = members_etmun_per10kInh,
         "URBACT\n10kHab" = members_urbact_per10kInh,
         "EUCICOP\n10kHab" = participations_eucicop_per10kInh)

df4 <- df3FR %>% select(c(31,37:39, 3:5, 16))
df4 <- df2 %>% select(3,5, 32,31,37:39,43, 44)%>% 
  tidylog::filter(members_etmun > 1 & participations_eucicop > 1)%>%
  select(-members_etmun, - participations_eucicop)
skim(df4)


df4 <- df2 %>% tidylog::filter(adminLevel != "other")   %>% select(31,37,,38, 40,50,51)
rownames(df4) <- df4$label
df4 <- df4 %>% select(-label)
res.pca <- PCA(df4,
               scale.unit = TRUE,
               #ind.sup = 7,
                #quanti.sup = 7,
               quali.sup = 5,
               graph = FALSE)



explor(res.pca)

CoordPCA<- as.data.frame(res.pca$ind$coord)

df2 <- cbind(df2, CoordPCA)

8125/16614
clean_name

dfzero <- df4 %>% filter_at(vars(4:6), any_vars(. == 0))
#PCA normalized on log value

df4 <- dfLogFilter %>% select(c(31, 43:48))
skim(df4)

rownames(df4) <- df4$label
df4 <- df4 %>% select(-label)
res.pca <- PCA(df4,
               scale.unit = TRUE,
               #ind.sup = df[ , 1],
               #quanti.sup = 6,
               #quali.sup = 5,
               graph = FALSE)
ggpairs(df4, 
        lower = list(continuous = wrap("points", alpha = 0.2)))
explor(res.pca)

lm1 <- lm( members_etmun ~ PopAdmin11 * countryCode,data = df2)
lm2 <- lm( participations_eucicop ~ PopAdmin11 * countryCode,data = df2)
summary(lm1)
summary(lm2)

lm3 <- lm( members_etmun_per10kInh ~ PopAdmin11 * countryCode,data = df2)
lm4 <- lm( participations_eucicop_per10kInh ~ PopAdmin11 * countryCode,data = df2)

summary(lm3)
summary(lm4)

lm5 <- lm( participations_eucicop ~ members_etmun * countryCode,data = df2)
lm6 <- lm( participations_eucicop_per10kInh ~ members_etmun_per10kInh * countryCode,data = df2)

summary(lm5)
summary(lm6)
# library(GGally)
# ggpairs(df2, columns = c(1:6))
# 
# hist(df2$members_urbact)
# myVar <- df2 %>% 
#   filter(members_urbact>0)
# summary(myVar$members_urbact)

#==== CAH sur ACP ====
library("factoextra")
indi <- get_pca_ind(res.pca)
indi <- as.data.frame(indi$coord)

df4 <- cbind(df4,indi)


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
cah <- ComputeClassif(df = df4,
                      varquanti = myVar, method = "ward", stand = FALSE)


dendro <- PlotDendro(classifobj = cah)
dendro
inert <- PlotHeight(classifobj = cah)
inert
myProfiles <- PlotProfile(classifobj = cah, nbclus = 6)


df2$cluster <- cutree(tree = cah, k = 4)
freq <- data.frame(table(df2$cluster))

## ==== linear regression ====

reg <- lm(participations_eucicop ~ members_etmun , data = sfCityEur)
summary(reg)


### Equation de la droite de regression :
eq = paste0("y = ", round(reg$coefficients[2],2), " * x + ", round(reg$coefficients[1],2))

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





#### ===K-means====
  #===
  
  # == K-means On standardized variables
  
  ## Normalized variable (filter london)
  dfKmean <- df2 %>% select(31,37:39,43)
rownames(dfKmean) <- dfKmean$label
dfKmean <- dfKmean %>% select(-label)
dfKmeanNorm <- dfKmean %>% mutate_all(~scale(.))
rownames(dfKmeanNorm) <- rownames(dfKmean)


#Log test
# dfKmeanLog <- dfLogFilter %>% select(3:5, 16,31) 
# dfKmeanLogNorm <- dfKmeanLog %>% select(-label) %>% mutate_all(~scale(.))
# rownames(dfKmeanLogNorm) <- dfKmeanLog $label
# 
# 
# ## Kmeans on ACP coord
# CoordPCA4 <- CoordPCA %>% select(-5)

## Compute explained variance (setting K) 
ggplot(df2 %>% filter(adminLevel != "other") %>% tidylog::filter(participations_eucicop > 1) , 
       aes(x = RankPop2011_Nat, y = participations_eucicop, color = adminLevel ) ) +
  scale_y_continuous(trans = "log10")+
  geom_point () +
  theme_light() +
  facet_wrap(~countryCode, scales = "free")

ggplot(df2 %>% filter(adminLevel != "other") %>% tidylog::filter(members_etmun > 1) , 
       aes(x = RankPop2011_Nat, y = members_etmun, color = adminLevel ) ) +
  scale_y_continuous(trans = "log10")+
  geom_point () +
  theme_light() +
  facet_wrap(~countryCode, scales = "free")
colnames(df2)

ggplot(df2 %>% filter(adminLevel != "other") %>% filter(members_etmun_per10kInh != 0), 
       aes(x = RankPop2011_Nat, y = members_etmun_per10kInh, color = adminLevel ) ) +
  geom_point () +
  theme_light() +
  facet_wrap(~countryCode, scales = "free")

ggplot(df2 %>% filter(adminLevel != "other") %>% filter(participations_eucicop_per10kInh != 0), 
       aes(x = RankPop2011_Nat, y = participations_eucicop_per10kInh, color = adminLevel ) ) +
  geom_point () +
  theme_light() +
  facet_wrap(~countryCode, scales = "free")

ggplot(testfilter, 
       aes(x = members_etmun, y = participations_eucicop, color = adminLevel )) +
  geom_point () +
  theme_light() +
  facet_wrap(~countryCode, scales = "free")


testfilter<- df2 %>% filter(adminLevel != "other") %>% tidylog::filter(participations_eucicop > 1 & members_etmun > 1)
var.expl <- c()
for (i in 1:40){
  result <- kmeans(x=dfKmeanNorm,
                   centers=i,
                   iter.max=100,
                   nstart=5)
  var.expl <- append(var.expl,result$betweenss/result$totss)
}

var.expl <- data.frame(n=1:40,
                       var=var.expl)


ggplot(var.expl, aes(x = n, y = var))+
  geom_point() + scale_x_continuous(breaks = seq(0,60,5)) +
  theme(plot.subtitle = element_text(vjust = 1),
        plot.caption = element_text(vjust = 1),
        plot.title = element_text(colour = "brown")) +
  labs(x = "k", y = "variance explained") +
  labs(title = "K-Means Clustering on Population Vectors ")

# Performing simple K-mean Partition

K=8
clusters <- kmeans(x=dfKmeanNorm,
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


clusLong <- clustersCenters %>% 
  select(-n) %>% 
  gather(key = "variable", value = "value", - Cluster) 

profilePlot <- ggplot(clusLong) +
  geom_bar(aes(x = variable, y = value), fill = "grey30", position = "identity", stat = "identity") +
  scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
  facet_wrap(~ Cluster) + coord_flip() + theme_bw()
profilePlot



plot(dfKmean,col=clusters$cluster)
points(clusters$center,col=1:2,pch=8,cex=1)
#### Add K-means clustering in df

OutputClustering <- as.data.frame(clusters$cluster)

colnames(OutputClustering)[1] <- "KmeansCluster"


dfKmean <- cbind(dfKmean, OutputClustering)
dfKmean <- cbind(dfKmeanNorm, OutputClustering)
## profils en box plots

t <- as.character(seq(1:max(dfKmean$KmeansCluster)))
# Reshaper les données

indicateursClasseTest <- dfKmean %>% mutate(KmeansCluster = as.factor(KmeansCluster))%>%
  mutate(KmeansCluster = fct_relevel(as.character(KmeansCluster), t))  %>% #t est un vecteur pour ordonner tes classes
  group_by(KmeansCluster) %>%
  mutate(ClusterLabel = paste0(KmeansCluster, " (n=" ,n(), ")")) %>% #on colle le nom du cluster et le nombre d'individus concernés
  ungroup() %>%
  gather(key = Variable, value = Valeur, -KmeansCluster, -ClusterLabel) %>% # enlevant les indicateurs qualii
  mutate(KmeansCluster = fct_relevel(KmeansCluster, t))



vartoplot <- colnames(dfKmean)[colnames(dfKmean) != "KmeansCluster"]
library(lemon)
ggplot(indicateursClasseTest %>% filter(Variable %in% vartoplot), aes(Variable, Valeur)) + 
  geom_boxplot(aes(fill = KmeansCluster), outlier.shape = NA ) + 
  facet_rep_wrap(~ClusterLabel, ncol = 3, repeat.tick.labels = F) + 
  ylab("Percentage by neighborhood") +
  xlab("") +
  scale_x_discrete(limits = vartoplot) +
  theme(legend.position = "none") +
  theme(axis.text.y = element_text(size = 8, angle = 0)) +
  theme(panel.spacing = unit(1, "cm")) +
  scale_y_continuous(breaks = seq(-5, 10, 5), limits = c(-5,10)) + # a regler selon la distri de tes variables
  theme(strip.text.x = element_text(size = 24))+
  theme(axis.text.x =   element_text(size = 24)) +
  theme(axis.text.y =   element_text(size = 10)) +
  scale_fill_viridis_d() +
  coord_flip()



dfKmeanMean <- dfKmean %>% group_by(KmeansCluster)%>% summarise_all(~mean(.))
dfKmeanMean <- dfKmeanMean %>% gather(key = "Variable", value = "Mean", - KmeansCluster)

profilePlot <- ggplot(dfKmeanMean) +
  geom_bar(aes(x = Variable, y = Mean), fill = "grey30", position = "identity", stat = "identity") +
  scale_x_discrete("Variable") + scale_y_continuous("Valeur moyenne par classe") +
  facet_wrap(~ KmeansCluster) + coord_flip() + theme_bw()
profilePlot


#### Add K-means clustering in df (log normalisé et filtre)

OutputClustering <- as.data.frame(clusters$cluster)

colnames(OutputClustering)[1] <- "KmeansClusterLog"


dfKmeanLog <- cbind(dfKmeanLog, OutputClustering)
dfKmeanMean <- dfKmeanLog %>% select(-label) %>% group_by(KmeansClusterLog)%>% summarise_all(~median(.))
dfKmeanMean <- dfKmeanMean %>% gather(key = "Variable", value = "Median", - KmeansClusterLog)

profilePlot <- ggplot(dfKmeanMean) +
  geom_bar(aes(x = Variable, y = Median), fill = "grey30", position = "identity", stat = "identity") +
  scale_x_discrete("Variable") + scale_y_continuous("Valeur médiane par classe (log)") +
  facet_wrap(~ KmeansClusterLog) + coord_flip() + theme_bw()
profilePlot

#### Plot with Mean Pop

library(tidyr)
meanPopK <- UMZ %>%
  select(7:12, KmeansPop) %>%
  group_by(KmeansPop) %>%
  summarise_each(funs(mean(., na.rm=TRUE))) %>%
  gather(key = year, value = Pop, -KmeansPop) %>%
  mutate(year = gsub(pattern = "X", replacement = "", x = year))

library(ggplot2)
ggplot(meanPopK, aes(year, Pop, group=KmeansPop, color = factor(KmeansPop))) +
  geom_line()




########### =====CAH TrajPop =====

library(ade4)
library(cluster)
library(FactoClass)
TrajPop <- df2 %>% tidylog::filter_at(vars(38:40), any_vars(is.finite(.))) %>% select(38:40)

rownames(TrajPop) <- df2$label
# AFC

AFC <- dudi.coa(df=TrajPop, scannf=FALSE, nf=ncol(TrajPop))
plot.dudi(AFC)


#Calcul matrice de distance au khi2

distMat <- dist.dudi(AFC, amongrow=TRUE)

# distMat <- as.data.frame(as.matrix(distMat))
#Calcul de la CAH ponderee

CAH <- ward.cluster(distMat,
                    plots = TRUE, h.clust = 1)

#poid CAH : , peso = apply(X=TrajPop, MARGIN=1, FUN=sum), 
#Graphiques descriptifs

par(mfrow=c(1,2))
barplot(sort(CAH$height / sum(CAH$height), decreasing = TRUE)[1:15] * 100,
        xlab = "Noeuds", ylab = "Share of total inertia (%)",
        names.arg=1:15, main="Inertie selon le partitionnement")

barplot(cumsum(sort(CAH$height / sum(CAH$height), decreasing = TRUE))[1:15] * 100,
        xlab = "Nombre de classes", ylab = "Part de l'inertie totale (%)",
        names.arg=1:15, main="Inertie expliquée")

#   library(NbClust)
#   NbClust(data = TrajPop,diss = distMat, distance = NULL, method = "ward.D2",min.nc = 2, max.nc = 15,index = "all")
# # Dendrogram
par(mfrow=c(1,1))
#
plot(as.dendrogram(CAH), leaflab = "none")


#Decoupe de la CAH

df2$TrajPopclusters <- cutree(tree = CAH, k = 4)

by_clusters <- df2 %>% group_by(TrajPopclusters) %>% mutate(n = n()) %>%
                group_by(n, add= TRUE) %>%
                summarise_at(vars(38:40), 
                             list(mean = ~mean(.),
                                  med = ~median(.)))


#Plot ClusterMean


meanprofil <- by_clusters %>%
  select(TrajPopclusters, n, ends_with("mean"))%>%
  pivot_longer(c(-TrajPopclusters,-n), names_to = "variable", values_to = "value")

profilePlot <- ggplot(meanprofil) +
  geom_bar(aes(x = TrajPopclusters, y = value, fill = as.character(TrajPopclusters)), stat = "identity") +
  scale_x_discrete("Classe") + scale_y_continuous("Valeur moyenne par classe") +
  scale_fill_discrete("Classe")+
  facet_wrap(~ variable,scales = "free") + coord_flip() + theme_bw()
profilePlot


medprofil <- by_clusters %>%
  select(TrajPopclusters, n, ends_with("med"))%>%
  pivot_longer(c(-TrajPopclusters,-n), names_to = "variable", values_to = "value")

profilePlot <- ggplot(medprofil) +
  geom_bar(aes(x = TrajPopclusters, y = value, fill = as.character(TrajPopclusters)), stat = "identity") +
  scale_x_discrete("Classe") + scale_y_continuous("Valeur médiane par classe") +
  scale_fill_discrete("Classe")+
  facet_wrap(~ variable,scales = "free") + coord_flip() + theme_bw()
profilePlot

relativeweight <-  UMZ %>%
  select(7:12, TrajPopclusters) %>%
  gather(key = year, value = Pop, -TrajPopclusters) %>%
  mutate(year = gsub(pattern = "X", replacement = "", x = year))%>%
  group_by(TrajPopclusters,year)%>%
  summarise(TotClustYear = sum(Pop))%>% ungroup()

relativeweight <- relativeweight%>%
  group_by(year)%>% mutate(TotYear = sum(TotClustYear))%>%
  mutate(Weight = (TotClustYear/TotYear)*100)


ggplot(relativeweight, aes(year, Weight, group=TrajPopclusters, color = factor(TrajPopclusters))) +
  geom_line()+ scale_colour_manual(breaks=c("1","2", "3", "4"),  
                                   values= colorTrajPopAll,
                                   labels=c("1. Croissance et ralentissement après 1990 (1446 UMZ)", 
                                            "2. Croissance forte et continue (330 UMZ)", 
                                            "3. Stagnation et décroissance (883 UMZ)", 
                                            "4. Croissance régulière et stable (1303 UMZ)"))+ labs(title = "Trajectoires démographiques des  agglomérations européennes (UMZ)"
                                                                                                   ,colour = "Cluster TrajPop", x = "Année", y ="Poids Relatif des clusters (% de la population urbaine totale)")

# write.csv2(UMZ, file = "CAH_UMZ_6Classes_Allemagne_10K.csv", row.names = F)
###### Plot AFC with class


plot.dudi(AFC, Tcol = TRUE, Trow = FALSE)

s.class(cstar=1,addaxes=TRUE, grid=FALSE, axesell=TRUE,
        dfxy=AFC$li, fac=as.factor(UMZ$TrajPopclusters), col=1:6,
        label=c(1:6), csub=1.2, possub="bottomright", add=TRUE)
bind

### ANOVA (cluster / city size)
UMZ$TrajPopclusters <- as.factor(UMZ$TrajPopclusters)
anova<- aov(X1961 ~ TrajPopclusters, data = UMZ)
anova
summary(anova)

ggplot(UMZ, aes(x=TrajPopclusters, y = log10(X1961)))+ geom_boxplot()

ggplot(UMZ, aes(x=TrajPopclusters, y = log10(X1961)))+ geom_violin(trim = T)+geom_boxplot(width = 0.1) + 
  labs(title= "Cities' 1961 Populations by type of TrajPop clusters", y = "Cities Population (log10)")

##" Chi 2



###=====ACM=====

df_acm <-df2 %>% 
  #filter(continentCode == "EU") %>% 
  select(31,23, 47:49, 51:53) %>% 
  drop_na()




# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/84-acm-dans-r-avec-factominer-scripts-faciles-et-cours/
library(FactoMineR)

colnames(df_acm) <- c("admin","etmun", "eucicop", "pop11", "urbact", "ratioetmun", "ratioeucicop")
dfACM<- Map(paste, df_acm, names(df_acm), sep = '_') %>% 
  as.data.frame() 

rownames(dfACM) <- df2$label

res.mca <- MCA(dfACM, graph=FALSE, quali.sup = c(1,4))
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
     autoLab = "no")

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


#### ===== country typo predictors =======


ggplot(df2, aes(y = participations_eucicop, x = LocGovType_VerticalPwrRelation, fill = LocGovType_VerticalPwrRelation))+ 
  geom_boxplot()+ facet_wrap(~popadmin11_K)
sciencepo <- colnames(df2[,c(24:29)])
participation <- colnames(df2[,c(3,5)])
ggduo(df2, sciencepo, participation, mapping = aes(color = popadmin11_K), legend = c(1,2), outlier.size   = 0)
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############# ==== GLM  NB and P =======
  library(MASS)
  dfmodel <- df %>% dplyr::select(c(1:5, 16, 23:29)) %>% 
    mutate(sq.Pop11 = sqrt(PopAdmin11))%>% 
    drop_na() %>% 
    filter(adminLevel != "other")
  skim(dfmodel)
  M1nb <- glm.nb(participations_eucicop ~ sq.Pop11 + members_etmun + subregion, 
                 link = "log", data =   dfmodel)
  M2nb <- glm.nb(participations_eucicop ~ sq.Pop11 + members_etmun + adminLevel*countryCode, 
                 link = "log", data =   dfmodel)

  summary(M1nb)mais 
  100* (28543-14479 )/28543
  summary(M2nb)

  ## ZINB
  library(pscl)
  f1 <- formula(participations_eucicop ~  adminLevel*subregion + sq.Pop11
                   | adminLevel*subregion + sq.Pop11)
  
  
  Nb1 <- zeroinfl(f1, dist = "negbin", link = "logit",
                  data = dfmodel)
summary(Nb1)  

verif <- as.data.frame(xtabs(~ factor(participations_eucicop > 0) + adminLevel + subregion, data = dfmodel))


skim(df$participations_eucicop)
skim(df$members_etmun)
me