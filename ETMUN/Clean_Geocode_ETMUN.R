
### Open Files
setwd("~/BD_Keep_Interreg/ETMUN")
library(readxl)

library(tidyverse)


path <- "DataSource/ETMUN_MemberCitiesCollect.xlsx"

MemberCities <- path %>% 
                excel_sheets() %>% 
                set_names() %>% 
                map(read_excel, path = path)
sapply(MemberCities, class)

### Extract the metadata
MetaData <- MemberCities[[1]]

## Keep only data
MemberCities <- MemberCities[-1]

## Convert logical columns
MemberCities <- lapply(MemberCities, transform, Year_of_Joining = as.character(Year_of_Joining)) 
MemberCities <- lapply(MemberCities, transform, Pop_collect = as.numeric(Pop_collect)) 
MemberCities <-  lapply(MemberCities, transform, Code_Network = as.character(Code_Network)) 

str(MemberCities[1])
## Bind all data in a dataframe

MemberCities <- MemberCities%>%map_df(bind_rows)

str(MemberCities)

MemberCities <- MemberCities[, -(15:39)]

###### Check DF
library(skimr)

skim(MemberCities)


MemberCities %>% filter(is.na(Full_Name))

MemberCities %>% filter(is.na(Locality_Siege))

MemberCities <- MemberCities %>% filter(!is.na(Locality_Siege))



########################################################################################################"
############################ Clean Dictionnary and Complete Localisation info

unique(MemberCities$Entity)

### Simplified entities 

MemberCities <- MemberCities %>% mutate(EntitySimplified = Entity)
MemberCities %>% filter(is.na(Entity) )
          # Municipality
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "Municipal Authority"  = "Municipality",
                             "Intermediary Municipal authority"= "Municipality",
                            "Municipal Administration" = "Municipality"  ))
          
          # Region County
          
          unique(MemberCities$EntitySimplified)
          
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "Regional Authorities"  = "Region",
                            "County" = "Region",
                            "Regional Authority" = "Region",
                            "County Authority" = "Region"))
          
          #Intermunicipality
          unique(MemberCities$EntitySimplified)
          
          MemberCities %>% filter(Entity ==  "Association of Municipalities")
         
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "Intermunicipal Authority"  = "Intermunicipality",
                            "Intermuncipality" = "Intermunicipality",
                            "Intercommunality" = "Intermunicipality",
                            "Intermunicipal Association" = "Intermunicipality",
                            "Association of Municipalities" = "Intermunicipality"))
          # Association
          
          unique(MemberCities$EntitySimplified)
   
    
          
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "Regional Supramunicipal Association"  = "Regional Association",
                            "Regional association"  = "Regional Association"))
          
          
          # National-state entity 
          unique(MemberCities$EntitySimplified)
          
          
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "State"  = "State Administration",
                            "Stade Body"  = "State Administration", 
                            "State Body" = "State Administration",
                            "Body State"   = "State Administration",
                            "National Authority" = "State Administration",))
          
          # Other 
          unique(MemberCities$EntitySimplified)
          
          MemberCities %>% filter(Entity == "Network Administration")
          
          MemberCities %>% filter(Entity == "District")%>% length()
          
          MemberCities <- MemberCities %>% 
            mutate(EntitySimplified =
                     recode(EntitySimplified, 
                            "Network Administration"  = "City",
                            "District"   = "City"))
          
          
unique(MemberCities$EntitySimplified)



#### Check Country code _ Recode
library(countrycode)
sort(unique(MemberCities$CountryCode))

# Recode ISO 2 mistake
MemberCities %>% filter(CountryCode ==  "ES/PT")

MemberCities <- MemberCities %>% mutate(CountryCode = recode(CountryCode, "ES/PT" = "ES",
                                                             "ES " = "ES", "USA" = "US", "UK" = "GB", "Perú" = "PE", "Kosovo" = "RKS",
                                                             "Portugal"  = "PT", 
                                                             "Brasil" = "BR",
                                                             "Argentina"  = "AR",
                                                             "Bolivia" = "BO",
                                                             "Chile"  = "CL",
                                                             "Colombia" = "CO",
                                                             "España" = "ES",
                                                             "Ecuador" = "EC",
                                                             "El Salvador" = "SV", 
                                                             "Guatemala" = "GT",
                                                             "Honduras" = "HN",
                                                             "México"  = "MX", 
                                                             "Nicaragua" = "NI", 
                                                             "Panamá"   = "PA" ,
                                                             "Paraguay" = "PY",
                                                             "República Dominicana" = "DO",
                                                             "Puerto Rico" = "PR", 
                                                             "Uruguay" = "UY","Venezuela" = "VE", "Cuba" = "CU", "AN" = "NL",
                                                             ))


sort(unique(MemberCities$CountryCode))

CountryDictionnary <- data.frame(Code = sort(unique(MemberCities$CountryCode)))
CountryDictionnary$Name <- countrycode(CountryDictionnary$Code,  'iso2c', 'country.name') 
# Make a full name for geocoding


MemberCities <- MemberCities %>% 
  mutate(CountryName = countrycode(CountryCode,  'iso2c', 'country.name'))

unique(MemberCities$CountryName)

skim(MemberCities)


###### Continent and region


MemberCities <- MemberCities %>% 
  mutate(Continent = countrycode(CountryCode,  'iso2c', 'continent'), Region = countrycode(CountryCode,  'iso2c', 'region') )

skim(MemberCities)


############################################################### Code Adhésion
##############################################################


MemberCities <- MemberCities %>% 
  mutate(MembershipCode = paste0("A" ,rownames(.))) 

skim(MemberCities)

##### Create a character vector for geocode

MemberCities <- MemberCities %>% 
  mutate(Adress = paste(Locality_Siege ,CountryName, sep = ", ")) 

skim(MemberCities)

LocationMember <- unique(MemberCities$Adress)

################################"" Geocode 
library(ggmap)

register_google(key = "")

ggcoord <- mutate_geocode(MemberCities, location = Adress )


skim(ggcoord)

#guess_encoding(ggcoord)

#write.csv2(ggcoord, file = "DataSource/MembersETMUNGeocode.csv", row.names = F, fileEncoding = "UTF-8")


#Get long lat in the main df
MemberCities <- MemberCities %>% left_join(select(ggcoord, MembershipCode, lon, lat), by = "MembershipCode") 


### try substitution of old geocode for NA geocode with ggmap
library(tidylog)

MemberCities <- MemberCities %>% mutate(lon = ifelse(is.na(lon), X, lon), lat = ifelse(is.na(lat), Y, lat)) 

skim(MemberCities)
# MemberCities$X = as.numeric(MemberCities$X)
# MemberCities$Y= as.numeric(MemberCities$Y)
# MemberCities$lon = as.numeric(MemberCities$lon)
# MemberCities$lat = as.numeric(MemberCities$lat)


NoGeocodeEtmun <- MemberCities %>% filter(is.na(lon))



### Check results


library(sf)
library(mapview)



EtmunPoints <- st_as_sf(MemberCities %>% filter(!is.na(lon)), coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry")

mapview(EtmunPoints%>% filter(Continent == "Europe"))



VerifEncoding <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F)
