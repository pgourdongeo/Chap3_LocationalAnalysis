
library(tidyverse)
library(tidylog)
library(skimr)
library(photon)
library(countrycode)
library(cartography)
library(sf)

DataRep <- path.expand ('Chap3_LocationalAnalysis/DataSource/')

list.files(DataRep)

Projects <- read.csv2("Chap3_LocationalAnalysis/DataSource/Keep_ClosedProject_LeadPartner_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(Projects)


Partners <- read.csv2("Chap3_LocationalAnalysis/DataSource/Keep_ClosedProject_Partner.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(Partners)


#### Geocoding Partner

Partners$Town <- trimws(Partners$Town)
Partners$Postal.code2 <- gsub("\\s", "", Partners$Postal.code)  
Partners$location <- paste(Partners$Postal.code,", ",Partners$Town,", ",Partners$Country, sep = "")

Partners$location2 <- paste(Partners$Town,", ",Partners$Country, sep = "")
PartnerLocation <- Partners$location %>% unique()

P <- sample(PartnerLocation, size = 10)


GeoCoord <- geocode(PartnerLocation, limit = 1)

skim(GeoCoord)


#write.csv2(GeoCoord, file = "GeoCoord.csv", row.names = F, fileEncoding = "UTF-8")
### Geocode remaining
# Get non-geocode entities
NoGeo <- GeoCoord %>% filter(!is.na(msg))
#Merge with partners to get location 2

NoGeoPartners <- NoGeo %>% left_join(Partners, by = "location")%>% filter(!duplicated(location))
NoGeoPartners <- NoGeoPartners %>% mutate(location3 = paste(Street,Postal.code, Town, Country, sep = ", "))

#NoGeo <- NoGeo %>% mutate(location2 = trimws(tolower(location)))
test <-sample_n(NoGeoPartners, size=10)

testreturn <- geocode(test$location2, limit = 1)
library(ggmap)
library(geonames)
options(geonamesUsername="pgourdon")
options(geonamesHost = "api.geonames.org/")
GNsearch(q =test$location3)

source(system.file("tests","testing.R",package="geonames"),echo=TRUE)

testreturn <- ggmap::geocode(test$location, output = "more" )
register_google(key = "X4QiiMqsB3iA7pQxkqNoNdPxA1sLDYt8")
NoGeo_Code <- ggmap::geocode(NoGeo$location, output = "more", source = "google", override_limit = F) 

NoGeo_Code <- NoGeo_Code %>% rename(location = address)
NoGeo_Code <- NoGeo_Code %>% mutate(location = as.character(location))

GeocodeAll <- GeoCoord %>% left_join(NoGeo_Code, by = "location")
GeocodeAll <- GeocodeAll %>% mutate(lon.x = ifelse(is.na(msg), lon.x,lon.y), lat.x = ifelse(is.na(msg), lat.x,lat.y))%>%
  mutate(SourceGeoCode = ifelse(is.na(msg), "OSM_photon","dsk_ggmap"))
PartnersGeoCode <- Partners %>% left_join(GeocodeAll, by = "location")

###### Vérif géocodage
##Recode country
unique(PartnersGeoCode$Country)
PartnersGeoCode <- PartnersGeoCode %>% mutate(CountrySimplified = case_when(Country == "French Guiana" ~ "France",
                                                                            Country == "Guadeloupe" ~ "France",
                                                                            Country == "Martinique" ~ "France",
                                                                            Country == "Reunion" ~ "France",
                                                                            Country== "Greenland" ~ "Denmark",
                                                                            Country == "Faroe Islands" ~ "Denmark", TRUE ~ as.character(Country) ))
unique(PartnersGeoCode$CountrySimplified)

PartnersGeoCode <- PartnersGeoCode %>%mutate(country.y = as.character(country.y))%>% mutate(CountryGeoCode = ifelse(is.na(country.x), country.y,country.x) )
skim(PartnersGeoCode)
unique(PartnersGeoCode$CountryGeoCode)

 PartnersGeoCode<- PartnersGeoCode %>% mutate(CountryGeocodeSimplified = case_when(CountryGeoCode == "French Guiana" ~ "France",
                                                                                   CountryGeoCode == "Guadeloupe" ~ "France",
                                                                                   CountryGeoCode == "Martinique" ~ "France",
                                                                                   CountryGeoCode == "Reunion" ~ "France",
                                                                                   CountryGeoCode== "Greenland" ~ "Denmark",
                                                                                   CountryGeoCode == "Faroe Islands" ~ "Denmark",
                                                                                   CountryGeoCode == "The Netherlands" ~ "Netherlands",
                                                                                   CountryGeoCode== "F.Y.R.O.M." ~ "Former Yugoslav Republic Of Macedonia",
                                                                                   CountryGeoCode == "Scotland"  ~ "United Kingdom",
                                                                                   CountryGeoCode == "UK" ~ "United Kingdom",
                                                                                   CountryGeoCode == "Slovak Republic" ~ "Slovakia",
                                                                                   CountryGeoCode == "Slovak Republic" ~ "Slovakia",
                                                                                   CountryGeoCode == "Slovak Republic" ~ "Slovakia",
                                                                                   TRUE ~ as.character(CountryGeoCode) ))

## Repérer les pays qui posent problème
ProblemCountry <- PartnersGeoCode %>%filter(!PartnersGeoCode$CountrySimplified == PartnersGeoCode$CountryGeocodeSimplified)
GoodGeoCodageAll <- PartnersGeoCode %>%filter(PartnersGeoCode$CountrySimplified == PartnersGeoCode$CountryGeocodeSimplified)
# Distance levenshtein entre les chararctères

ProblemCountry <- ProblemCountry%>%  mutate(distLVChr = stringdist(ProblemCountry$CountrySimplified,ProblemCountry$CountryGeocodeSimplified, method = "lv"),
                                            distCosineChr = stringdist(ProblemCountry$CountrySimplified,ProblemCountry$CountryGeocodeSimplified, method =  "cosine"))
summary(ProblemCountry$distLVChr)
summary(ProblemCountry$distCosineChr)

### Géocodage manuel
ProblemCountry_GeoCode <- ggmap::geocode(ProblemCountry$location2, output = "more", source = "dsk") 
#write.csv2(ProblemCountry, file= "ProblemCountry_ManualGeocodage.csv", row.names = F, fileEncoding = "UTF-8")

ProblemCountry_GeoCode <- ProblemCountry_GeoCode %>% rename(address = addres)
ProblemCountry_GeoCode <- ProblemCountry_GeoCode %>% mutate(location = as.character(location))

ProblemCountry_GeoCodeNamed <- ProblemCountry_GeoCode %>% rename(lon.y = lon, lat.y = lat,country.y = country )

replace_imputed <- function(original, imputed){
  
  namestoChange <- colnames(original)[colnames(original) %in% colnames(imputed)]
  
  for(i in 1:length(namestoChange)){
    original[namestoChange[i]] <- imputed[namestoChange[i]]
  }
  return(original)
  
}
colnames(ProblemCountry)[colnames(ProblemCountry) %in% colnames(ProblemCountry_GeoCodeNamed)]
ProblemCountry_Recode <- replace_imputed(ProblemCountry, ProblemCountry_GeoCodeNamed)

ProblemCountry_Recode %>% filter(SourceGeoCode=="dsk_ggmap")

# str(ProblemCountry_GeoCode)
# GeocodeAll_Country <- PartnersGeoCode %>% filter(!duplicated(location))%>% select(location,Postal.code,Town,TownLowerCase, Country, Country.code, NUTS1..or.equivalent.,NUTS2..or.equivalent., NUTS3..or.equivalent.,c(19:46))
# 


### Jointure + selection des villes (group by region et name , select first)

# skim(PartnersGeoCode)
# unique(PartnersGeoCode$TownLowerCase)
# PartnersGeoCode <- PartnersGeoCode %>% mutate(TownLowerCase = tolower(Town))
# g<-PartnersGeoCode %>%select(TownLowerCase, Country, NUTS2..or.equivalent., lon.x,lat.x)%>% group_by(TownLowerCase, Country)%>% top_n(n=1, wt=TownLowerCase)


## Get Population (admin )
# City <- Partners$Town %>% unique()
# Partner
# library(httr)
# library(jsonlite)
# urlygi <- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldcitiespop&q=paris&sort=population&facet=country"
# blob <- fromJSON(urlygi)
# blob$records -> foo
# 
# baseURL <- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldcitiespop&q={ville}&sort=population&facet=country&geofilter.distance={lat}%2C{long}%2C{distance_max}"
# library(glue)
# foo <- fromJSON(glue(baseURL, ville = "Mol", long = 5.115527, lat = 51.18404, distance_max = 10000)) %>%
#   magrittr::extract2("records")
# url <- "https://public.opendatasoft.com/api/records/1.0/search/?dataset=worldcitiespop&q=paris&sort=population&facet=country&geofilter.distance=48.866667%2C2.333333%2C10000"
# 

#### Spatial Joint (intersect ou within)

PointGeocode <- st_as_sf(GeocodeAll, coords = c("lon.x", "lat.x"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)
PointGeocode
mapview(PointGeocode)

list.files("TradeveShape/")
Tradeve <- read_sf("TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp")%>%st_set_crs(3035)
Tradeve
mapview(Tradeve)


SpatialJoint <- st_within(PointGeocode, Tradeve, sparse = T)
length(SpatialJoint[1,])

head(SpatialJoint)
class(SpatialJoint)


LogicalJoint<-lengths(SpatialJoint) > 0

PctUMZjoint <-(sum(LogicalJoint)/ length(LogicalJoint))*100
PctNoUMZjoint <- (1-(sum(LogicalJoint)/ length(LogicalJoint)))*100

SpatialJoint_UMZ_Geocode <- st_join(PointGeocode, Tradeve, join = st_within)
skim(SpatialJoint_UMZ_Geocode)
SpatialJoint_UMZ_Geocode <- SpatialJoint_UMZ_Geocode %>% select(-c(2:26))



PartnerUMZ <- Partners %>% left_join(SpatialJoint_UMZ_Geocode, by= "location")

skim(PartnerUMZ)
ProjectsByUMZ <- PartnerUMZ %>% filter(!is.na(OBJECTID))%>% group_by(OBJECTID)%>% mutate(Nparticpation = n())%>% ungroup()
skim(ProjectsByUMZ)
ProjectsByUMZunique <- ProjectsByUMZ %>% filter(!duplicated(OBJECTID))%>% select(-c(1:15))
summary(ProjectsByUMZunique$Nparticpation)
gg1<- ggplot(ProjectsByUMZunique, aes(x = Nparticpation, y = Pop2011)) + geom_point()
gg1
# valeurs possibles pour trans : 'log2', 'log10','sqrt'
gg1+ scale_y_continuous(trans='log10')


########### Density
library(sp)
SP_PartnersAll <- st_as_sf(PartnersGeoCode, coords = c("lon.x", "lat.x"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>%
  st_transform(crs = 3035)
SP_PartnersAll$dummies<- 1
str(SP_PartnersAll)


?smoothLayer
  CRS(SP_PartnersAll)

  SP_PartnersAll_SP <- as(SP_PartnersAll, "Spatial") 

  
  Mescouleurs<- carto.pal(pal1 ="wine.pal",n1= 8 )
  opacity <- 85
  Mescouleurs<-  paste0(Mescouleurs,opacity) 
  
font <- getTiles(SP_PartnersAll_EU, type = "stamenbw",crop = F)
  tilesLayer(font)
  plot(SP_PartnersAll_EU$geometry,add=T)
str(SP_PartnersAll_SP)
smoothLayer(x= SP_PartnersAll_EU, var="dummies", 
            typefct = "exponential", span=80000, beta=2, resolution =10000, mask = NULL,
            nclass = 8, breaks = NULL, col = Mescouleurs, border = "grey20", lwd = 1,
            legend.pos = "bottomleft", legend.title.txt = "Potential",
            legend.title.cex = 0.8, legend.values.cex = 0.6, legend.values.rnd = 0,
            legend.frame = FALSE, add = T)


SP_PartnersAll$continent <- countrycode(SP_PartnersAll$Country, origin = "country.name", destination = "continent")

SP_PartnersAll_EU <- SP_PartnersAll %>% filter(continent == "Europe")%>% filter(Country == "France" )%>% filter(country.x== "France")%>% filter(!state== "Réunion")

mapview(SP_PartnersAll_EU)
unique(SP_PartnersAll_EU$Country)
unique(SP_PartnersAll_EU$country.x)
mapview(SP_PartnersAll)

unique(SP_PartnersAll$Country)
