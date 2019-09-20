#Packages

library(skimr)
library(tidyverse)
library(stringdist)
## Load Data
setwd("~/BD_Keep_Interreg/KEEP/AD")
list.files()
Partners <- read.csv2("PartnersGeoCode.csv" , encoding = "UTF-8", stringsAsFactors = F)

skim(Partners)

### Sample

SamplePartners <- sample_n(Partners, 120)


### Compute string dist on name (test)

stringdistmatrix(SamplePartners$Project.Partner, method = "cosine")


vecchar <- c("Turin", "Torino", "Torun", "Athen Municipality", "Municipality of Athen", "Athne", "Athen")

TestMetricdistString <-function(vecchar){
  results <- list()
  
  results[["hamming"]] <-stringdistmatrix(vecchar, method="hamming", useNames = TRUE)
  results[["qgram"]]  <-stringdistmatrix(vecchar, method="qgram", useNames = TRUE)
  results[["cosine"]] <-stringdistmatrix(vecchar, method="cosine", useNames = TRUE)
  results[["jaccard"]] <-stringdistmatrix(vecchar, method="jaccard", useNames = TRUE)
  results[["lcs"]]   <-stringdistmatrix(vecchar, method="lcs", useNames = TRUE)
  results[["lv"]]  <-stringdistmatrix(vecchar, method="lv", useNames = TRUE)
  results[["osa"]] <-stringdistmatrix(vecchar, method= "osa", useNames = TRUE)
  
  return(results)
  
}




### Forme de la fonction => récupérer les 10 plus proches voisins
### Compute cosine distance with threshold

# Récupérer les partners unique
UniquePartners <- Partners %>% distinct(ID_PARTNER, .keep_all= TRUE)

### Faire un sf en 4326
library(sf)
library(spdep)
#
PointPartners <- st_as_sf(UniquePartners, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") #%>%
  #st_transform(crs = 3035)
PointPartners


coords_Partners <- st_coordinates(PointPartners)


k10partners <- knn2nb(knearneigh(coords_Partners, k=10, longlat = TRUE), row.names = PointPartners$ID_PARTNER)


distnei <- nbdists(k10partners, coords_Partners, longlat = TRUE)
class(distnei)

distnei <- unlist(distnei)

summary(distnei)


#### Test sur un jeu de voisins

k10partners[[1]]
length(k10partners)
test10voisin <- UniquePartners %>% filter(rownames(.) %in%  c(1,k10partners[[1]]))
matchname10nei <- amatch(test10voisin$Project.Partner[1],test10voisin$Project.Partner[-1], method="cosine", maxDist = 0.5)
matchname10nei

matchname10nei2 <-
## 8ème voisin candidat 

### Test pour pouvoir regrouper les listes avec des éléments communs

testlist<- list(c(1,2), c(2,1,3))

result <- testlist %>% 
  # check whether any numbers of an element are in any of the elements
  map(~map_lgl(testlist, compose(any, `%in%`), .x)) %>% 
  unique() %>%    # drop duplicated groups
  map(~reduce(testlist[.x], union))   



########### Function
library(svMisc)

MergingCandidate <- function(sf,namevar = sf$VarName,k =10,maxDistCosine =0.5){
  require(tidyverse, attach.required = TRUE)
  require(sf, attach.required = TRUE)
  require(spdep, attach.required = TRUE)
  require(stringdist, attach.required = TRUE)
  require(svMisc)
  coords_sf <- st_coordinates(sf)
  
  kn10 <- knn2nb(knearneigh(coords_Partners, k=k, longlat = TRUE))
  
  results <- list()
  
  for(i in c(1: length(kn10))){
    
    INeigh <- sf %>%  filter(rownames(.) %in%  c(i,kn10[[i]]))
    results[[i]] <- c(i, amatch(INeigh$VarName[1],INeigh$VarName[-1], method="cosine", maxDist = maxDistCosine))
 
      progress(i, progress.bar = TRUE)
      Sys.sleep(3)
      if (i == length(kn10)) cat("Done!\n")
    }
  
  result <- results %>% 
    # check whether any numbers of an element are in any of the elements
    map(~map_lgl(results, compose(any, `%in%`), .x)) %>% 
    unique() %>%    # drop duplicated groups
    map(~reduce(results[.x], union)) 
  
  return(result)
}


### test function


sf <- sample_n(Partners, 30)


sf<-sf %>% distinct(ID_PARTNER, .keep_all= TRUE)

#
sf <- st_as_sf(sf, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") 



MergingCandidate(PointPartnerstest, namevar = PointPartnerstest$Project.Partner,k=10, maxDistCosine = 0.5)

coords_sf <- st_coordinates(sf)

kn10 <- knn2nb(knearneigh(coords_Partners, k=5, longlat = TRUE))

results <- list()

for(i in c(1: length(kn10))){
  
  INeigh <- sf %>%  filter(rownames(.) %in%  c(i,kn10[[i]]))
  results[[i]] <- c(i, amatch(INeigh$VarName[1],INeigh$VarName[-1], method="cosine", maxDist = 0.5))
  
  progress(i, progress.bar = TRUE)
  Sys.sleep(3)
  if (i == length(kn10)) cat("Done!\n")
}

result <- results %>% 
  # check whether any numbers of an element are in any of the elements
  map(~map_lgl(results, compose(any, `%in%`), .x)) %>% 
  unique() %>%    # drop duplicated groups
  map(~reduce(results[.x], union)) 
