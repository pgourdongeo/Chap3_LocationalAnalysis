
library(tidyverse)
library(skimr)
#Load data
setwd("~/BD_Keep_Interreg/KEEP")

PartnersID <- read.csv2("AD/PartnersGeoCode.csv", stringsAsFactors = F)


PartnersOriginal <- read.csv2("DataSource/Keep_ClosedProject_Partner.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

Projects <- read.csv2("DataSource/Keep_ClosedProject_LeadPartner_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

skim(PartnersID)
skim(PartnersOriginal)
PartnersOriginal[PartnersOriginal==""]<-NA
PartnersID[PartnersID==""]<-NA
# Make NA in Partnersoriginal postal code


### First joint beetween partners geocoded and partners original files to avoid encoding issues

    # Concatenate Acronym and Project Partner and other variables, filter the 6 duplicated (mistake in the KEEP database)
PartnersID <- PartnersID %>% mutate(Join = paste(Acronym, Project.Partner,Postal.code, sep="_"))%>% filter(!duplicated(Join))
    
PartnersOriginal <- PartnersOriginal %>% mutate(Join = paste(Acronym, Project.Partner,Postal.code, sep="_"))%>% filter(!duplicated(Join))

#identical(sort(PartnersID$Join), sort(PartnersOriginal$Join))
skim(PartnersID)
skim(PartnersOriginal)

    # join files with the new column
Partners <- PartnersOriginal %>% 
  left_join(select(PartnersID, Join, ID_PARTICIPATION, ID_PARTNER), by = "Join")

skim(Partners)
summary(is.na(Partners$ID_PARTICIPATION))


# 3153 entities with no ID, because of character encoding issues 

PartnersNoId <- Partners %>% filter(is.na(ID_PARTICIPATION))

VecMatchID <- Partners %>% filter(!is.na(ID_PARTICIPATION))%>% select(ID_PARTICIPATION)%>% deframe()

PartnersIDpb <- PartnersID %>%filter(! ID_PARTICIPATION %in% VecMatchID )
        # Perform a fuzzy join (too long crash R)
        
        # library(fuzzyjoin)
        
        # testfuzzy <- stringdist_join(PartnersNoId, PartnersIDpb, by = "Join",method = "cosine", max_dist= 0.3, mode = "left" ) 

## Function to get match (source : https://r.789695.n4.nabble.com/Fuzzy-Matching-td4671603.html)


# Example data
# string.tobematched <- c("ENRON CORPORATION", "DOW JONES CHEMICAL", "ANNA", "DOW JONES CHEM", "DOVILE", "DOW JONES EXPLOR")
# string.complete <- c("ANA", "DOV", "DOW JONES CHEM", "DOW JONES CHEMICAL", "DOW JONES", "DOW JONES EXPLORATION", "ENRON",
#                      "ENRON INC", "XYZ", "PLP", "TNT", "ENRON CORP", "DOW")

string.tobematched <- PartnersNoId$Join
string.complete <- PartnersIDpb$Join

#This code below is practically the same as above, but it's much faster. I matched 6,000 names to my database (as mentioned, 50,000 rows) in approximately 6 hours. I'm sure I could get rid of the for loop, but I've been in a hurry and needing to get the job done fast (any suggestions would be greatly appreciated!).

# Jaro Winkler Distance for Text Matching (useful e.g. for merging databases based only on names that don't match exactly)
# This is significantly faster than previous code, as it eliminates naming the jwproximity matrix's rows and columns
# string.tobematched and string.complete are used as much as possible, not row and column names!
# PROBLEM: is that if there's more than 1 max match for a particular name, the columns for that row in the csv file are shifted

library(RecordLinkage)

# Function
myjarowinkler <- function(string.tobematched, string.complete){
  
  jwproximity <- matrix(data = NA, nrow = length(string.tobematched), ncol = length(string.complete))
  
  for(i in 1:nrow(jwproximity)){
    for(j in 1:ncol(jwproximity)){
      jwproximity[i,j] <- jarowinkler(string.tobematched[i], string.complete[j])
    }}  
  
  max.jwproximity <- data.frame(string.tobematched,
                                as.character(apply(jwproximity, 1, function(x) string.complete[which(x == max(x))])),
                                apply(jwproximity, 1, which.max) + 1,
                                apply(jwproximity, 1, max), stringsAsFactors = FALSE)
  colnames(max.jwproximity) <- c("Name_to_be_Matched", "Closest_Match_Found",
                                 "Index_of_Closest_Match", "Max_Jaro_Winkler")
  
  write.csv(max.jwproximity,"MatchIDPartners.csv",fileEncoding = "UTF-8")
  return(max.jwproximity)
}

matched<- myjarowinkler(string.tobematched, string.complete)

skim(matched)

### Join 


PartnersNoId <- PartnersNoId %>% left_join(matched, by = c("Join"= "Name_to_be_Matched"))
skim(PartnersNoId)

PartnersGetID <- PartnersNoId %>%select(-ID_PARTICIPATION,-ID_PARTNER) %>% left_join(select(PartnersIDpb, ID_PARTICIPATION, ID_PARTNER, Join), by = c("Closest_Match_Found" = "Join"))

skim(PartnersGetID)

PartnersGetIDGood <- PartnersGetID %>%filter(Max_Jaro_Winkler>0.67) 

### Bind the table to get the clean dataset with ID

PartnersClean <- Partners %>%filter(!is.na(ID_PARTICIPATION))

PartnersGetIDGood <- PartnersGetIDGood %>% select(-Closest_Match_Found, - Max_Jaro_Winkler, - Index_of_Closest_Match)

PartnersClean <- rbind(PartnersClean, PartnersGetIDGood)

skim(PartnersClean)

PartnersClean %>% filter(is.na(ID_PARTICIPATION))
duplicate <-PartnersClean%>% group_by(ID_PARTICIPATION) %>% filter(n() > 1)
### Drop 60 duplicated (encoding error)
PartnersCleanFinal <- PartnersClean %>% filter(!is.na(ID_PARTICIPATION))%>% select(-Join)%>% filter(!duplicated(ID_PARTICIPATION))
skim(PartnersCleanFinal)
write.csv2(PartnersCleanFinal, "DataSource/PartnersWithID.csv", row.names= F, fileEncoding = "UTF-8 ")


### Get Long Lat

load("AD/Keep_ClosedProject_Partner_corrected.RDS")

Partner$ID_PARTICIPATION <- as.character(Partner$ID_PARTICIPATION)
PartnersCleanFinalCoord <- PartnersCleanFinal %>% left_join(select(Partner, ID_PARTICIPATION, lon, lat), by = "ID_PARTICIPATION")

skim(PartnersCleanFinalCoord)

write.csv2(PartnersCleanFinalCoord, "DataSource/PartnersWithIDGeocoord.csv", row.names= F, fileEncoding = "UTF-8 ")



######################################################################################################
########################### JOIN PARTNERS PROJECTS DATABASE ##########################################""


ProjectsID <- Projects %>% mutate(ref = paste( Acronym, Programme, sep = "_"))

UniqueRefProj <- ProjectsID %>% select(ref)%>% unique() %>% deframe()
UniqueRefProj <- data.frame(ref = UniqueRefProj)

UniqueRefProj$ID_PROJECT <- paste0("PR", row.names(UniqueRefProj))
UniqueRefProj$ref <- as.character(UniqueRefProj$ref)


ProjectsID <- ProjectsID %>% left_join(UniqueRefProj, by = "ref")

skim(ProjectsID)

PartnersCleanFinalCoordKProject <- PartnersCleanFinalCoord %>% mutate(ref = paste(Acronym, Programme, sep = "_") )
PartnersCleanFinalCoordKProject <- PartnersCleanFinalCoordKProject %>% left_join(select(ProjectsID, ref, ID_PROJECT), by ="ref")%>%distinct()

skim(PartnersCleanFinalCoordKProject)

PartnersCleanFinalCoordKProject[677,]

ProjectsID[16757,]


#### Final files


ProjectsIDFinal <- ProjectsID %>% select(-ref)

write.csv2(ProjectsIDFinal, "DataSource/ProjectsID.csv", row.names= F, fileEncoding = "UTF-8 ")


PartnersProjectsKey <- PartnersCleanFinalCoordKProject %>% select(-ref)
write.csv2(PartnersProjectsKey, "DataSource/PartnersIDProj.csv", row.names= F, fileEncoding = "UTF-8 ")
saveRDS(Projects, file = "DataSource/ProjectsID.rds")
saveRDS(Partners, file = "DataSource/PartnersIDProj.rds")
