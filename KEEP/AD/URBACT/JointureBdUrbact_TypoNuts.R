
setwd("~/Thèse/BD_Réseaux_Villes/Jointures_Base/JointureBdUrbact_Typo Nuts")

library(tidyverse)
library(sf)
library(readr)
list.files()

guess_encoding("BdCitiesUrbact_Code_UMZ_LAU2.csv", n_max = 1000)

BdUrbactCities <- read.csv("BdCitiesUrbact_Code_UMZ_LAU2.csv",
                           header = T,
                           stringsAsFactors = F,
                           sep = ";",
                           encoding = "UTF-8")


NUTS_UrbainRural <- read_sf("NUTS_TypoRuralUrbain.shp")%>% st_transform(3035)


NUTS_CP_2014_2020 <- read_sf("NUTS2_CohesionPolicy_2014_2020.shp")%>% st_transform(3035)

NUTS_CP_2006_2013 <-read_sf("NUTS2_EuropeanFunds_2006_2013.shp")%>% st_transform(3035)

Europe <- read_sf("CNTR_RG_60M_2010.shp")%>% st_transform(3035)

UrbactCities0713 <- BdUrbactCities %>% filter(Start.x < 2013)

UrbactCities1420 <- BdUrbactCities %>% filter(Start.x >= 2013)

#### Rang des villes par réseaux



##### Données aggrégées


UrbactCitiesAggr <- BdUrbactCities %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1,0)) %>%
  select(CodeCity, Name,X,Y, Country, Region.x, Continent.x,POPLAU2_2015,ID_UMZ,Pop2011,Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% select(-Lead)%>%
  distinct()%>%filter(!duplicated(CodeCity))

              

UrbactCitiesAggr0713 <- UrbactCities0713 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1,0)) %>%
  select(CodeCity, Name,X,Y, Country, Region.x, Continent.x,POPLAU2_2015,ID_UMZ,Pop2011,Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct()%>%filter(!duplicated(CodeCity))

UrbactCitiesAggr1420 <- UrbactCities1420 %>%
  mutate(Lead = ifelse(City.Statut == "Lead Partner", 1,0)) %>%
  select(CodeCity, Name,X,Y, Country, Region.x, Continent.x,POPLAU2_2015,ID_UMZ,Pop2011,Lead) %>%
  group_by(CodeCity) %>%
  mutate(NbParticipation = n(), NbLeader = sum(Lead)) %>% 
  distinct()%>%filter(!duplicated(CodeCity))


###### Datapoint


UrbactCitiesPoints <- st_as_sf(UrbactCitiesAggr, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

UrbactCitiesPoints0713 <- st_as_sf(UrbactCitiesAggr0713, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

UrbactCitiesPoints1420 <- st_as_sf(UrbactCitiesAggr1420, coords = c("X", "Y"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)

####### Jointures spatiales


UrbactCities_TypoUrbRural <- st_join(UrbactCitiesPoints,NUTS_UrbainRural, 
                join = st_intersects, left = T)


UrbactCities_TypoPC0713 <-  st_join(UrbactCitiesPoints0713,NUTS_CP_2006_2013, 
                                    join = st_intersects, left = T)

UrbactCities_TypoPC1420 <- st_join(UrbactCitiesPoints1420,NUTS_CP_2014_2020, 
                                   join = st_intersects, left = T)




#### Code variable Urbain rural
NUTS_UrbainRural$Typo_8Clv1<- as.factor(NUTS_UrbainRural$Typo_8Clv1)
NUTS_UrbainRural <- NUTS_UrbainRural %>% 
                    mutate(Typo8 = recode(Typo_8Clv1, "1" = "Régions sous dominance\n d'une métropole",
                                          "2"= "Régions avec densité\n urbain/rurale élevées",
                                          "3" = "Régions avec densité\n urbaine élevée",
                                          "4" = "Régions à majorité\n de villes moyennes",
                                          "8" = "Régions rurales\n sous influence métropolitaine",
                                          "5" = "Régions rurales\n sous influence de grandes villes",
                                          "6" = "Regions rurales\n avec villes petites et moyennes",
                                          "7" = "Régions rurales isolées"))

UrbactCities_TypoUrbRural <- UrbactCities_TypoUrbRural %>%
  mutate(Typo8 = recode(Typo_8Clv1, "1" = "Sous dominance\n d'une métropole",
                        "2"= "Densité\n urbain/rurale\n élevées",
                        "3" = "Densité\n urbaine élevée",
                        "4" = "Majorité\n de villes moyennes",
                        "8" = "Rurales\n sous influence\nmétropolitaine",
                        "5" = "Rurales\n sous influence\nde grandes villes",
                        "6" = "Rurales\n avec villes \npetites et moyennes",
                        "7" = "Régions rurales\nisolées"))
                            


### Code blank in eligibility data



## Carto simple

library(cartography)
library(ggplot2)

plot(Europe$geometry, border = "black", col = "grey")

#### Carto typo urbain rural 



#plot(NUTS_UrbainRural$geometry, border = "grey10", col = colUrbRurl, add = T)


typoLayer(NUTS_UrbainRural,
           var = "Typo8",
          col = c("darkred", "darkorange", "firebrick1","darksalmon","gold","lightyellow","darkolivegreen1","darkolivegreen4" ),
          legend.values.order = c("Régions sous dominance d'une métropole",
                                  "Régions avec densité urbain/rurale élevées",
                                  "Régions avec densité urbaine élevée",
                                   "Régions à majorité de villes moyennes",
                                   "Régions rurales sous influence métropolitaine",
                                  "Régions rurales sous influence de grandes villes",
                                   "Regions rurales avec villes petites et moyennes",
                                   "Régions rurales isolées"),
           border = "grey20", # color of the polygons borders
           lwd = 0.5, # width of the borders
           legend.pos = "topleft", # position of the legend
           legend.title.txt = "Typo Rural Urbain", # title of the legend
           add = TRUE) 


### Carto typo nuts eligibility

plot(Europe$geometry, border = "black", col = "grey")




typoLayer(NUTS_CP_2006_2013,
          var = "TYPE",
          col = c("dodgerblue3", "deepskyblue", "lightyellow","darkorange","grey80" ),
          legend.values.order = c("Competitiveness and Employment",
                                  "Phasing-in regions",
                                  "Phasing-out regions",
                                  "Convergence regions", "Outside Typology"),
          border = "grey20", # color of the polygons borders
          lwd = 0.5, # width of the borders
          legend.pos = "topleft", # position of the legend
          legend.title.txt = "Structural Funds Eligibility 2006-2013", # title of the legend
          add = TRUE) 


plot(Europe$geometry, border = "black", col = "grey")




typoLayer(NUTS_CP_2014_2020,
          var = "TYPE",
          col = c("deepskyblue", "darkorange","firebrick1","grey80" ),
          legend.values.order = c("More developed regions",
                                  "Transition regions",
                                  "Less developed regions",
                                   "Outside Typology"),
          border = "grey20", # color of the polygons borders
          lwd = 0.5, # width of the borders
          legend.pos = "topleft", # position of the legend
          legend.title.txt = "Structural Funds Eligibility 2014-2020", # title of the legend
          add = TRUE) 


#####################


TabCroisUrbainRural <- UrbactCities_TypoUrbRural %>% 
                        group_by(Typo8) %>%
                        summarise(N = n(), nParticipation = sum(NbParticipation), nLeaderCity = sum(NbLeader))%>%
                        mutate(Typo8 = as.character(Typo8))

sum(TabCroisUrbainRural$nParticipation)
TabCroisUrbainRural[9,1] <- "Outside Typology"

TabCroisNUTS0713 <- UrbactCities_TypoPC0713 %>%
                    group_by(TYPE) %>%
                    summarise(N= n(), nParticipation = sum(NbParticipation), nLeaderCity = sum(NbLeader))

TabCroisNUTS0713[3,1] <- "Outside Typology" 
TabCroisNUTS1420 <- UrbactCities_TypoPC1420 %>%
                    group_by(TYPE) %>%
                    summarise(N= n(), nParticipation = sum(NbParticipation), nLeaderCity = sum(NbLeader))

TabCroisNUTS1420[3,1] <- "Outside Typology"
library(tidyr)
TabCroisUrbRuralPlot <- TabCroisUrbainRural %>%
                        gather(key = "Type", value= "NB", nParticipation,nLeaderCity)%>%
                        group_by(Type)%>%
                        mutate(Pct = (NB/sum(NB))*100, Ratio = NB/N)

ggplot(TabCroisUrbRuralPlot, aes(x = reorder(Typo8,-Ratio), y = Ratio, fill =Typo8 )) + 
  geom_bar(stat = "Identity")+ facet_wrap(~Type, scales = "free")+
  labs(title = "Ratio of involvments in URBACT Networks by type of regions",
       x="Regional typology Urban/Rural", y = "Ratio (N participations/N types of regions)")+
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1), 
    axis.text = element_text(vjust = 0.25), 
    axis.text.x = element_text(size = 8 ))+
  scale_fill_discrete(name="Urban/Rural typology\n of NUTS (2 & 3)", breaks = c("Sous dominance\n d'une métropole",
                    "Densité\n urbain/rurale\n élevées","Densité\n urbaine élevée","Majorité\n de villes moyennes", 
                    "Rurales\n sous influence\nmétropolitaine", "Rurales\n sous influence\nde grandes villes",
                      "Rurales\n avec villes \npetites et moyennes", "Régions rurales\nisolées", "Outside Typology"), 
                    labels= c("Sous dominance d'une métropole",
                              "Densité urbaine/rurale élevées","Densité urbaine élevée","Majorité de villes moyennes", 
                              "Rurales sous influence métropolitaine", "Rurales sous influence de grandes villes",
                              "Rurales avec villes petites et moyennes", "Régions rurales isolées", "Outside Typology"))+
 theme(axis.title = element_text(size = 14), 
    plot.title = element_text(size = 16), 
    legend.text = element_text(size = 11), 
    legend.title = element_text(size = 14))


ggplot(TabCroisUrbRuralPlot, aes(x = reorder(Typo8,-Pct), y = Pct,fill = Type )) + 
  geom_bar(stat = "Identity",position = "dodge")+
  labs(title = "Percentage of involvments in URBACT Networks by types of regions",
       x="Typology Urban/Rural", y = "Percentage of total participation")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 1), 
        axis.text.x = element_text(size = 8 ))+
  scale_fill_discrete(name="Type of Involvment",
                      breaks=c("nLeaderCity", "nParticipation"),
                      labels=c("Leader Partner", "All Participations"))+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))
# ggsave(plot = last_plot(), "test.png", width = 30, height = 20, units = "cm", dpi = 150)


TabCroisNUTS0713Plot <- TabCroisNUTS0713 %>%
  gather(key = "Type", value= "NB", nParticipation,nLeaderCity)%>%
  group_by(Type)%>%
  mutate(Pct = (NB/sum(NB))*100, Ratio = NB/N)%>% filter(!TYPE == "NA")

ggplot(TabCroisNUTS0713Plot, aes(x = reorder(TYPE,-Pct), y = Pct,fill = Type )) + 
  geom_bar(stat = "Identity",position = "dodge")+
  labs(title = "Percentage of involvments in URBACT Networks by types of regions",
       x="Structural Funds Eligibility 2006-2013", y = "Percentage of total participation")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 1), 
        axis.text.x = element_text(size = 10))+
scale_fill_discrete(name="Type of Involvment",
                    breaks=c("nLeaderCity", "nParticipation"),
                    labels=c("As Lead Partner", "All Participations"))+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))


ggplot(TabCroisNUTS0713Plot, aes(x = reorder(TYPE,-Ratio), y = Ratio, fill = TYPE )) + 
  geom_bar(stat = "Identity")+ facet_wrap(~Type, scales = "free")+
  labs(title = "Ratio of involvments in URBACT Networks by types of regions",
       x="Structural Funds Eligibility 2006-2013", y = "Ratio (N participations/N type of regions)")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 0.25),
        axis.text.x=element_blank())+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))

TabCroisNUTS1420Plot <- TabCroisNUTS1420 %>%
  gather(key = "Type", value= "NB", nParticipation,nLeaderCity)%>%
  group_by(Type)%>%
  mutate(Pct = (NB/sum(NB))*100, Ratio = NB/N)%>% filter(!TYPE == "NA")

ggplot(TabCroisNUTS1420Plot, aes(x = reorder(TYPE,-Pct), y = Pct,fill = Type )) + 
  geom_bar(stat = "Identity",position = "dodge")+
  labs(title = "Percentage of involvments in URBACT Networks by type of regions",
       x="Structural Funds Eligibility 2014-2020", y = "Percentage of total participation")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 1), 
        axis.text.x = element_text(size = 10))+
  scale_fill_discrete(name="Type of Involvment",
                      breaks=c("nLeaderCity", "nParticipation"),
                      labels=c("As Lead Partner", "All Participations"))+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))

ggplot(TabCroisNUTS1420Plot, aes(x = reorder(TYPE,-Ratio), y = Ratio )) + 
  geom_bar(stat = "Identity")+ facet_wrap(~Type, scales = "free")+
  labs(title = "Ratio of involvments in URBACT Networks by types of regions",
       x="Structural Funds Eligibility 2014-2020", y = "Ratio (N participations/N type of regions)")+
  theme(plot.subtitle = element_text(vjust = 1), 
        plot.caption = element_text(vjust = 1), 
        axis.text = element_text(vjust = 0.25), 
        axis.text.x = element_text(size = 10 ))+
  theme(axis.title = element_text(size = 14), 
        plot.title = element_text(size = 16), 
        legend.text = element_text(size = 11), 
        legend.title = element_text(size = 14))


####### Nparticipation et Ncities in regards of urbanisation (n UMZ)

UMZall <- read.csv("UMZ_Europe_1961_2011_Clean_Article.csv" ,
                header=TRUE,
                sep=";",
                dec=",",
                stringsAsFactors = FALSE)
UMZall$Country <- substr(UMZall$Country, 1,2)

N_UMZCountry <- UMZall %>% group_by(Country)%>% summarise(NUMZCountry = n())

N_CitiesParticipationCountry<- UrbactCitiesAggr %>% group_by(Country)%>% 
                                  summarise(Countryparticipations = sum(NbParticipation), 
                                            Ncities_Country = n())

Participations_ByCountry <- N_CitiesParticipationCountry %>% left_join(N_UMZCountry)

Participations_ByCountry <- Participations_ByCountry %>% filter(!Country =="NO")

Participations_ByCountry <- Participations_ByCountry %>% mutate(PctParticipationUMZ = (Countryparticipations/NUMZCountry)*100,
                                                                PctUrbactCitiesUMZ = (Ncities_Country/ NUMZCountry)*100)

Participations_ByCountry <- Participations_ByCountry%>% filter(!PctUrbactCitiesUMZ>100)

ggplot(Participations_ByCountry, aes(x = reorder(Country,-PctUrbactCitiesUMZ), y = PctUrbactCitiesUMZ )) + 
  geom_bar(stat = "Identity")+
  labs(title = "Share of cities involved in URBACT by urban morphological zones",
       x="Country", y = "Percentage Cities involved by UMZ")
#############################################################################################################""
##############################################################################################################
####################################""          TYPOLOGIE Réseau - Taille de ville    ############################
###############################################################################################################"
################################################################################################################"




RésuméStatRéseauxUMZ <- BdUrbactCities %>% 
                      group_by(Code_Network) %>% 
                      summarise(MeanUMZ = mean(Pop2011), 
                                MedUMZ = median(Pop2011),
                                VarUMZ = var(Pop2011),
                                SdUMZ = sd(Pop2011),
                                CvUMZ = SdUMZ/MeanUMZ,
                                MinUMZ = min(Pop2011),
                                MaxUMZ = max(Pop2011))

RésuméStatRéseauxUMZ <- as.data.frame(RésuméStatRéseauxUMZ)
row.names(RésuméStatRéseauxUMZ) <- RésuméStatRéseauxUMZ[,1]

RésuméStatRéseauxUMZ <- RésuméStatRéseauxUMZ[,-1]


library(GGally)



ggpairs(RésuméStatRéseauxUMZ)

RésuméStatRéseauxUMZ <- RésuméStatRéseauxUMZ %>% select(-SdUMZ,-MaxUMZ)
library(cluster)

CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "average")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "single")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "complete")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "ward")


plot(as.dendrogram(CAH), leaflab = "none")

RésuméStatRéseauxUMZ$CAH <- cutree(tree= CAH, k = 6)

RésuméMeanCAH <- RésuméStatRéseauxUMZ %>% ungroup() %>% 
              group_by(CAH)%>%
              summarise(N = n(),
                        Mean = mean(MeanUMZ),
                        Med = mean(MedUMZ),
                        Var = mean(VarUMZ),
                        #Sd = mean(SdUMZ),
                        Cv = mean(CvUMZ),
                        Min = mean(MinUMZ)
                       # Max = mean(MaxUMZ))
              )
              

#### Classement par seuil et proportion


summary(BdUrbactCities$Pop2011)
summary(BdUrbactCities$POPLAU2_2015)


## def des seuils
library(cluster)
labelClass <- c("4.Small City", "3.Medium-sized City", "2.Large City", "1.Very Large City")



cesDonnees <- BdUrbactCities[,22] 
min<-min(cesDonnees)
max<- max(cesDonnees)
valBreaks <- c(min,50000,150000,300000,max)

BdUrbactCities$ClassePopLAU<- cut(cesDonnees,
                   breaks = valBreaks,
                   labels = labelClass,
                   include.lowest = TRUE,
                   right= FALSE)


labelClass <- c("4.Small Urban Area", "3.Medium-sized Urban Area", "2.Large Urban Area", "1.Very Large Urban Area")
cesDonnees <- BdUrbactCities[,25] 
min<-min(cesDonnees)
max<- max(cesDonnees)
valBreaks <- c(min,100000,400000,1000000,max)

BdUrbactCities$ClassePopUMZ<- cut(cesDonnees,
                                  breaks = valBreaks,
                                  labels = labelClass,
                                  include.lowest = TRUE,
                                  right= FALSE)


#### Tableau résume  réseau / classe de taille

PropClassTailleUMZ <- BdUrbactCities %>% 
                      group_by(Code_Network,ClassePopUMZ)%>%
                      summarise(N = n())%>%
                      ungroup()%>%
                      spread(ClassePopUMZ, N)%>%
                      replace(is.na(.),0)%>%
                      mutate(NbCity = rowSums(.[2:5]))%>%
                      mutate_at(.,.vars= vars(2:5),.funs= funs(Pct = ((./NbCity)*100)))



PropClassTailleLAU <- BdUrbactCities %>% 
                      group_by(Code_Network,ClassePopLAU)%>%
                      summarise(N = n())%>%
                      ungroup()%>%
                      spread(ClassePopLAU, N)%>%
                      replace(is.na(.),0)%>%
                      mutate(NbCity = rowSums(.[2:5]))%>%
                      mutate_at(.,.vars= vars(2:5),.funs= funs(Pct = ((./NbCity)*100)))


         
PctCitySizeUMZ <- PropClassTailleUMZ %>% 
                  select(Code_Network,`4.Small Urban Area_Pct`:`1.Very Large Urban Area_Pct`)

PctCitySizeUMZ <- as.data.frame(PctCitySizeUMZ)
row.names(PctCitySizeUMZ) <- PctCitySizeUMZ[,1]
PctCitySizeUMZ <- PctCitySizeUMZ[,-1]

PctCitySizeLAU <- PropClassTailleLAU %>%
                  select(Code_Network,`4.Small City_Pct`:`1.Very Large City_Pct`)

PctCitySizeLAU <- as.data.frame(PctCitySizeLAU)
row.names(PctCitySizeLAU) <- PctCitySizeLAU[,1]
PctCitySizeLAU <- PctCitySizeLAU[,-1]


#### CAH

#CAH UMZ
CAH2 <- agnes(x = PctCitySizeUMZ, metric="euclidean", method = "ward")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "single")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "complete")
# CAH <- agnes(x = RésuméStatRéseauxUMZ, metric="euclidean", method = "ward")


plot(as.dendrogram(CAH2), leaflab = "none")

PropClassTailleUMZ$CAH <- cutree(tree= CAH2, k = 3)


UMZSizeClasseCAH <- PropClassTailleUMZ %>% ungroup()%>%
  group_by(CAH)%>%
  summarise(NbNetwork = n(),
            '1.Small_Urban_Areas' = mean(`4.Small Urban Area_Pct`),
            '2.MediumSized_Urban_Areas' = mean(`3.Medium-sized Urban Area_Pct`),
           '3.Large_Urban_Areas' = mean(`2.Large Urban Area_Pct`),
           '4.VeryLarge_Urban_Areas' = mean(`1.Very Large Urban Area_Pct`))
library(ggplot2)

UMZSizeClasseCAH <- UMZSizeClasseCAH %>%
  gather(Type, Pct,  -CAH,-NbNetwork)
ggplot(UMZSizeClasseCAH,aes(x=Type, y=Pct, fill = Type))+
  geom_col()+ facet_wrap(~CAH)
#CAH LAU

CAH2 <- agnes(x = PctCitySizeLAU, metric="euclidean", method = "ward")

plot(as.dendrogram(CAH2), leaflab = "none")

PropClassTailleLAU$CAH <- cutree(tree= CAH2, k = 6)

LAUSizeClasseCAH <- PropClassTailleLAU %>% ungroup()%>%
  group_by(CAH)%>%
  summarise(NbNetwork = n(),
            '1.Small_City' = mean(`4.Small City_Pct`),
            '2.MediumSized_City' = mean(`3.Medium-sized City_Pct`),
            '3.Large_City' = mean(`2.Large City_Pct`),
            '4.VeryLarge_City' = mean(`1.Very Large City_Pct`))



LAUSizeClasseCAH <- LAUSizeClasseCAH %>%
                    gather(Type, Pct,  -CAH,-NbNetwork)
ggplot(LAUSizeClasseCAH,aes(x=Type, y=Pct, fill = Type))+ geom_col()+ 
  labs(title = "Typology of Urbact Network Composition by City-size categories",
       subtitle = "Hierachical Clustering Analysis (Ward)",
       x="City Size Classes", y = "Percentage")+facet_wrap(~CAH)+ 
  scale_x_discrete(labels= NULL) + 
  theme(plot.subtitle = element_text(vjust = 1), 
    plot.caption = element_text(vjust = 1))+
  labs(subtitle = "Hierachical Clustering Analysis (Ward)")
