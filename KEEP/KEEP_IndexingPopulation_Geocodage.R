
library(tidyverse)
library(tidylog)
library(skimr)
library(photon)
library(countrycode)
library(cartography)
library(sf)

DataRep <- path.expand ('DataSource/')

list.files(DataRep)

Projects <- read.csv2("DataSource/Keep_ClosedProject_LeadPartner_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(Projects)


Partners <- read.csv2("DataSource/Keep_ClosedProject_Partner.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(Partners)


#### Geocoding Partners (all entities involved)

# Work on addresses (retrieve unique addresses from the 30. 000 partners )

Partners$Town <- trimws(Partners$Town)
Partners$Postal.code2 <- gsub("\\s", "", Partners$Postal.code)  
Partners$location <- paste(Partners$Postal.code,", ",Partners$Town,", ",Partners$Country, sep = "")

Partners$location2 <- paste(Partners$Town,", ",Partners$Country, sep = "")
PartnerLocation <- Partners$location %>% unique()

##Test geocode on a sample first
#P <- sample(PartnerLocation, size = 10)


GeoCoord <- geocode(PartnerLocation, limit = 1)

skim(GeoCoord)


#write.csv2(GeoCoord, file = "GeoCoord.csv", row.names = F, fileEncoding = "UTF-8")
### Geocode remaining
# Get non-geocode entities
NoGeo <- GeoCoord %>% filter(!is.na(msg))
#Merge with partners to get location 2

NoGeoPartners <- NoGeo %>% left_join(Partners, by = "location")%>% filter(!duplicated(location))
NoGeoPartners <- NoGeoPartners %>% mutate(location3 = paste(Street,Postal.code, Town, Country, sep = ", "))
## TEST adresses to lowercase
#NoGeo <- NoGeo %>% mutate(location2 = trimws(tolower(location)))
test <-sample_n(NoGeoPartners, size=10)



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


## Function to replace colum of a valid dataframe into an original one
replace_imputed <- function(original, imputed){
  
  namestoChange <- colnames(original)[colnames(original) %in% colnames(imputed)]
  
  for(i in 1:length(namestoChange)){
    original[namestoChange[i]] <- imputed[namestoChange[i]]
  }
  return(original)
  
}
colnames(ProblemCountry)[colnames(ProblemCountry) %in% colnames(ProblemCountry_GeoCodeNamed)]
ProblemCountry_Recode <- replace_imputed(ProblemCountry, ProblemCountry_GeoCodeNamed)



