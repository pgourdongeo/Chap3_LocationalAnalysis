
##==========================================================================##         
#                                 BD city - Jointur UIA
#                          
# DESCRIPTION : jointure entre les projets UIA et les bases Urbact et DB city

#
#
# PG, avril 2020
##==========================================================================##         

# Working directory huma-num
setwd("~/BD_Keep_Interreg/CITY")

library(tidyverse)
library(tidylog)
library(skimr)
library(readr)
library(sf)


UIA <- read.csv2("Data/UIAProjects_WebscrapApril2020.csv", stringsAsFactors = FALSE)

URBACT <- readRDS("CorrectedDB/URBACT_Membership_GNidCorr.RDS")

SfDBCity <- readRDS("Data/DBCity_LauUmzFua.rds")

DBCITY <- SfDBCity %>% st_drop_geometry()


## Filter DB city in EU space

iso <- c("IE", "GB", "PT", "ES", "FR", "BE", "NL", "LU", "DE", "DK",
         "AT", "IT", "GR", "SI", "HR", "CZ", "PL", "SK", "HU", "BG", 
         "RO", "EE", "LT", "LV", "CY", "FI", "SE", "MT")
DBCityEur <- DBCITY %>% filter(countryCode %in% iso)


#### ===== Join with DB City 

# UIAParticip <- UIA %>% group_by(City)%>% summarise(ParticipIUA = n())
# ## Test joint on LAU name
# UIAJoin <- UIAParticip %>% left_join(select(DBCITY, geonameId, asciiName, members_etmun, members_urbact, 
#                                     participations_eucicop, CODE_LAU, NAME_LAU, PopAdmin11), by = c("City" = "NAME_LAU"))
# 
# 
# UIAJoin %>% filter(duplicated(City))
# ## Breda (NL) and Lille (FR) duplicated 
# UIAJoin <- UIAJoin %>% slice(-14, -35)
# 
# 
# NoJoin <- UIAJoin %>% filter(is.na(geonameId))
# 
# 
# 
# UIAToComplete <- UIA %>% left_join(select(UIAJoin,City, geonameId, asciiName), by = "City")
# 
# write.csv2(UIAToComplete, "Data/UIAGNTocomplet.csv", row.names = F, fileEncoding = "UTF-8")

##### ====== Joint with GN id (manually corrected/joint)

UIAGnId <- read.csv2("Data/UIAparticip_GNid.csv", stringsAsFactors = F)

UIAGnId <- UIAGnId %>% mutate(geonameId = as.character(geonameId))

summary(UIAGnId$Budget)
UIAParticip <- UIAGnId %>% group_by(geonameId)%>%
  mutate(ParticipIUA = n()) %>% 
  select(geonameId, NAMECity_UIA = City, ParticipIUA) %>% 
  distinct()


UIAJoin <- UIAParticip %>% left_join(select(DBCITY, geonameId, asciiName, members_etmun, members_urbact, 
                                           participations_eucicop, CODE_LAU, NAME_LAU, PopAdmin11), 
                                     by = "geonameId")

unique(UIAJoin$members_urbact) 


UIAJoin <- UIAJoin %>% replace_na(list(members_etmun = 0, members_urbact = 0, participations_eucicop = 0))
UIAJoin <- UIAJoin %>% 
  mutate(urbact_discret = case_when(members_urbact == 0 ~ "sans participation",
                                    members_urbact == 1 ~ "participation unique",
                                    members_urbact == 2 ~ "double participations",
                                    TRUE ~ "plus de 2 participations"))
skim(UIAJoin%>% ungroup())

bibi <- table(UIAJoin$urbact_discret)
bibi2 <- as.data.frame(prop.table(bibi))
bibi2 <- bibi2 %>% 
  mutate(myOrder = case_when(Var1 == "sans participation" ~ 1,
                             Var1 == "participation unique" ~ 2,
                             Var1 == "double participations" ~ 3,
                             TRUE ~ 4),
         Freq = (Freq * 100) %>% round(., 0))


freq <- ggplot(bibi2, aes(x = reorder(Var1, myOrder), y = Freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Participations au programme URBACT", 
       y = "Pourcentage de villes du programme UIA") +
  labs(caption = "Sources : UIA, 2020 ; EUCICOP-URBACT 2019 / PG, AD, 2020") +
  theme_light() +
  theme(plot.caption = element_text(size = 6))

## display and save
pdf(file = "OUT/freq_uia_urbact.pdf", width = 8.3, height = 5.8)
freq
dev.off()

### Compare DB city EU and UIA  participations

MeanDB <- c(mean(DBCityEur$members_etmun), mean(DBCityEur$members_urbact),mean(DBCityEur$participations_eucicop))
MedDB <- c(median(DBCityEur$members_etmun), median(DBCityEur$members_urbact),median(DBCityEur$participations_eucicop))

MeanUIA <- c(mean(UIAJoin$members_etmun), mean(UIAJoin$members_urbact),mean(UIAJoin$participations_eucicop))
MedUIA <- c(median(UIAJoin$members_etmun), median(UIAJoin$members_urbact),median(UIAJoin$participations_eucicop))
Var <- c("Etmun Memberships", "Urbact Participations", "Eucicop Participations")

DfUIADbCity <- data.frame(Formula = c(rep("Mean", 6), rep( "Median", 6)), 
                          Variable = rep(Var, 4),
                          Data =c(rep("EU", 3), rep("UIA", 3), rep("EU", 3), rep("UIA", 3)),
                          Value = c(MeanDB, MeanUIA, MedDB, MedUIA), stringsAsFactors = F)

g <- ggplot(DfUIADbCity, aes(x =Variable, y = Value, fill = Data))+
  geom_bar(stat = "identity", position = "dodge")+ 
  facet_wrap(~ Formula)

g

DFcompare <- DfUIADbCity %>% 
  pivot_wider(names_from = c("Formula", "Variable"), values_from = Value, names_sep="_") 

VarName <- c("Ensemble de Référence" , 
             "Moyenne d'Adhésions\naux associations ETMUN", 
             "Moyenne de Participations\nau programme URBACT",
             "Moyenne de Participations\nEUCICOP",
             "Médiane d'Adhésions\naux associations ETMUN", 
             "Médiane de Participations\nau programme URBACT",
             "Médiane de Participations\n EUCICOP")
colnames(DFcompare) <- VarName

DFcompare <- DFcompare %>% 
  mutate_at(vars(2:4), ~ round(., 2))

DFcompare$"Nombre de villes" <- c("16 614", "67") 

DF <- as.data.frame(t(DFcompare))

colnames(DF) <- c("Ensemble des\nVilles de l'UE", "Villes du\nprogramme UIA")

DF$Variable <- rownames(DF)

DF <- DF %>% 
  select(Variable,everything() )

DF <- DF[-1,]
library(gridExtra)
tt <- ttheme_minimal(base_size = 6)
pdf(file = "OUT/df_CompareUIAparticipation.pdf", width = 8.3, height = 5.8)
grid.table(DF, rows = NULL) 
dev.off()


