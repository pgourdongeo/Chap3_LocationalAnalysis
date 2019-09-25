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

SamplePartners <- sample_n(Partners, 10)


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

## Test neighbbos functions
# coords_Partners <- st_coordinates(PointPartners)
# 
# 
# k10partners <- knn2nb(knearneigh(coords_Partners, k=10, longlat = TRUE), row.names = PointPartners$ID_PARTNER)
# d5kmNeir <- dnearneigh(coords_Partners,d1 = 0, d2 =5, longlat = TRUE)
# 
# 
# distnei <- nbdists(k10partners, coords_Partners, longlat = TRUE)
# class(distnei)
# 
# distnei <- unlist(distnei)
# 
# summary(distnei)


#### Test sur un jeu de voisins

# k10partners[[1]]
# length(k10partners)
# test10voisin <- UniquePartners %>% filter(rownames(.) %in%  c(1,k10partners[[1]]))
# testD20voisin <- UniquePartners %>% filter(rownames(.) %in%  c(1,d20kmNeir[[1]]))
# matchname10nei <- amatch(test10voisin$Project.Partner[1],test10voisin$Project.Partner[-1], method="cosine", maxDist = 0.5)
# matchname10nei
# 

## 8ème voisin candidat 

########### Function


MergingCandidate <- function(sf,
                             namevar, 
                             k= 10,
                             maxDistCosine = 0.5, 
                             controlVar, 
                             ID){
  #Load Package needed
  require(tidyverse, attach.required = TRUE)
  require(sf, attach.required = TRUE)
  require(spdep, attach.required = TRUE)
  require(stringdist, attach.required = TRUE)

  
  coords_sf <- st_coordinates(sf)#extract coordinates
  
  kn10 <- knn2nb(knearneigh(coords_sf, k= k, longlat = TRUE))## Compute the k nearest neighbors list. Take a long time.
 
  results <- list()
  
  for(i in c(1: length(kn10))){
    
    INeigh <- sf %>%  
      filter(rownames(.) %in%  c(i,kn10[[i]])) %>%  # get the reference entity and its neighbors. Longer for a sf object ? 
      filter(controlVar == controlVar[1])  ## Only keep Neigbors that have the same control variable than the reference entity
    
    Match <- amatch(INeigh$VarName[1],INeigh$VarName[-1], method="cosine", maxDist = maxDistCosine)# Find match under the cosine distance threshold
    
    NeigSame <- INeigh %>% filter(rownames(.) %in% Match) %>% 
      select(ID)%>% 
      as.data.frame()%>% 
      select(-geometry) %>% 
      deframe()#Vector with the neighbors that matched with the i entitu, by their ID
    
    VecPos  <- rownames(sf[sf$ID %in% NeigSame,])# rownames of the original df for the neighbors that potentially match
    
    results[[i]] <- c(i, VecPos)
    }
  results2 <-  results %>%   discard(~length(.x) <= 1)   # drop list element with no similar neighbors 
    
  FinalMatch <- results2 %>% # Melt results : if element 1 is similar to its neighbor 2, and element 2 is similar to element 3. Then 1, 2, 3 are considered to be potential same entity
    map(~map_lgl(results2, compose(any, `%in%`), .x)) %>%  # check whether any numbers of an element are in any of the elements
    unique() %>%    # drop duplicated groups
    map(~reduce(results2[.x], union)) 
  FinalMatch
  return(FinalMatch)
}


### test function


sf <- sample_n(Partners, 50)


sf<-sf %>% distinct(ID_PARTNER, .keep_all= TRUE)

#
sf <- st_as_sf(sf, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") 

sf <- PointPartners

MergingCandidate(sf , namevar = "Project.Partner",k=3 , maxDistCosine = 0.3, controlVar = "Country",
                ID = "ID_PARTNER")

coords_sf <- st_coordinates(sf)

kn10 <- knn2nb(knearneigh(coords_sf, k=4, longlat = TRUE))

results <- list()

for(i in c(1: length(kn10))){

  INeigh <- sf %>%  filter(rownames(.) %in%  c(i,kn10[[i]]))%>% filter(Country == Country[1])

  Match <- amatch(INeigh$Project.Partner[1],INeigh$Project.Partner[-1], method="cosine", maxDist = 0.3)

NeigSame <- INeigh %>% filter(rownames(.) %in% Match) %>%
    select(ID_PARTNER)%>%
    as.data.frame()%>%
    select(-geometry) %>%
    deframe()

 VecPos  <- rownames(sf[sf$ID_PARTNER %in% NeigSame,])

 results[[i]] <- c(i, VecPos)
}
results2 <-  results %>%   discard(~length(.x) <= 1)

result <- results2 %>%
  # check whether any numbers of an element are in any of the elements
  map(~map_lgl(results2, compose(any, `%in%`), .x)) %>%
  unique() %>%    # drop duplicated groups
  map(~reduce(results2[.x], union))


#### Create df of verification


df1 <- PointPartners %>%  filter(rownames(.) %in%  result[[1]])


