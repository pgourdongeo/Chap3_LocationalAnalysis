###############################################################################
#                                DATA PREPARATION KEEP
#
# DESCRIPTION : Preparation of KEEP downloaded files (row bind, 
# identify projects / partners / partnership
# geocoding and geonames query)
# PG
# November 2019
##############################################################################

############################
####     I/ Unique files (partners and projects)
###########################

### Packages
library(tidyverse)
library(skimr)
library(tidylog)

####Load data
setwd("~/BD_Keep_Interreg/KEEP")

list.files("DataSource")
# 2000-2006
Partners0006 <- read.csv2("DataSource/keep_closed_0006_Partnership.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

Projects0006 <- read.csv2("DataSource/keep_closed_0006_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

#2007-2013
Partners0713 <- read.csv2("DataSource/keep_closed_0713_Partnership.csv", stringsAsFactors = F)

Projects0713 <- read.csv2("DataSource/keep_closed_0713_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")

#2014-2020
Partners1420 <- read.csv2("DataSource/keep_closed_1420_Partnership.csv", stringsAsFactors = F)

Projects1420 <- read.csv2("DataSource/keep_closed_1420_Project.csv", stringsAsFactors = F, fileEncoding = "UTF-8")


### ################################ 1. R binds all files  ############################################################"
#################
####### PROJECTS
################
Projects <- bind_rows(Projects0006, Projects0713)

skim(Projects)

Projects <- bind_rows(Projects, Projects1420)

skim(Projects)

# Transform budget into numeric
Projects$Budget <- as.numeric(Projects$Budget)


########### Create a project ID
Projects <- Projects %>% mutate(ref = paste( Acronym, Programme, sep = "_"))

#Make a vector of unique project (cause duplicated possible : if one project as more than 1 lead partners)
UniqueRefProj <- Projects %>% select(ref)%>% unique() %>% deframe()
UniqueRefProj <- data.frame(ref = UniqueRefProj)

UniqueRefProj$ID_PROJECT <- paste0("PR", row.names(UniqueRefProj))
UniqueRefProj$ref <- as.character(UniqueRefProj$ref)


Projects <- Projects %>% left_join(UniqueRefProj, by = "ref")

skim(Projects)
## Check doublon id project
doublonProject <-Projects %>% group_by(ID_PROJECT)%>% filter(n() > 1)


#################
####### PARTNERS
################

Partners <- bind_rows(Partners0006, Partners0713)

skim(Partners)

Partners <- bind_rows(Partners, Partners1420)

skim(Partners)

########### Create a partnership ID
Partners <- Partners %>% mutate(ID_PARTICIPATION= paste0("p", row.names(.)))


########### Create a partner ID (unique partner = name + country. See documentation on KEEP FAQ Partner/Partnership)
Partners <- Partners %>% mutate(ref = paste( Project.Partner, Country, sep = "_"))


#Make a vector of unique Partner (as many duplication as the partner participated to different project)
UniqueRefPart <- Partners %>% select(ref)%>% unique() %>% deframe()
UniqueRefPart <- data.frame(ref = UniqueRefPart)

UniqueRefPart$ID_PARTNER <- paste0("P", row.names(UniqueRefPart))
UniqueRefPart$ref <- as.character(UniqueRefPart$ref)


Partners <- Partners %>% left_join(UniqueRefPart, by = "ref")

skim(Partners)

############ Add the ID_Project to Partners df

Partners <- Partners %>% mutate(ref = paste( Acronym, Programme, sep = "_"))
skim(Partners)
#Same number of unique value between Partners$ref and UniqueRefProj

Partners <- Partners%>% left_join(UniqueRefProj, by = "ref")

############################################## EXPORT CSV

### Projects with duplicated and lead partners info
Projects <- Projects %>% select(-ref)
write.csv2(Projects, "Data/ProjectsEucicop_all_LeadPartnerDuplicata.csv", row.names = F,fileEncoding = "UTF-8")

## Project without lead partner info and duplicata

ProjectsNoDuplicated <- Projects %>% select(-Lead.Partner, - Street, -Town, -Country) %>% filter(!duplicated(ID_PROJECT))

ProjectsNoDuplicated %>% distinct()

write.csv2(ProjectsNoDuplicated , 
           "Data/ProjectsEucicop_all_noduplicated.csv", 
           row.names = F,
           fileEncoding = "UTF-8")

### Partners
Partners <- Partners %>% select(-ref)

write.csv2(Partners , 
           "Data/PartnersEucicop_All.csv", 
           row.names = F,
           fileEncoding = "UTF-8")



