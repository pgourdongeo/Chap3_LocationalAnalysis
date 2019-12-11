
library(tidyverse)
library(skimr)
#Load data
setwd("~/BD_Keep_Interreg/KEEP")

Projects <- read.csv2("Data/ProjectsEucicop_all_noduplicated.csv",stringsAsFactors = F)


Partners <- read.csv2( "Data/Participations_All_Eucicop.csv", stringsAsFactors = F)


ParticipByPartners <- Partners %>% group_by(ID_PARTNER)%>% summarise(NParticip = n())

hist(ParticipByPartners$NParticip)
summary(ParticipByPartners$NParticip)
ParticipByPartners %>% filter(NParticip> 1) %>% summary()
skim(Projects)

# 
unique(Projects$Period) 
unique(Partners$Legal.Status)

Partners <- Partners %>% mutate(Legal.Status = recode(Legal.Status, "public" = "Public", "private" =  "Private", "n/a"= "NA" ))

unique(Partners$Legal.Status)
Partners <- Partners %>% mutate(Legal.Status = ifelse(Legal.Status == "", NA, Legal.Status))%>% 
  mutate(Legal.Status = ifelse(Legal.Status == "NA", NA, Legal.Status))

TypePartners <- Partners %>% group_by(ID_PROJECT, Legal.Status) %>% summarise(nMemberStatus = n()) %>% 
  spread(key = Legal.Status, value = nMemberStatus)%>% mutate_all(funs(replace_na(.,0)))

TypePartnersCntr <- Partners %>% group_by(Country, ID_PROJECT) %>% summarise(nCountry = n()) %>%
  mutate(Ntot = sum(nCountry))
  

TypePartners$TotalPartners <- rowSums(TypePartners[,sapply(TypePartners, is.numeric)])

summary(TypePartners$TotalPartners)
ggplot(TypePartners, aes(x = TotalPartners)) + geom_histogram(color="black", fill="white", binwidth=1)  +   
  geom_vline(aes(xintercept=mean(TotalPartners)), color="red", linetype="dashed", size=1)                               

TypePartners <- TypePartners %>% left_join(select(Projects, ID_PROJECT, Co.Financing.sources, Period, Programme), by = "ID_PROJECT")%>% distinct()

TypePartnersCntr <-  TypePartnersCntr %>% left_join(select(Projects, ID_PROJECT, Co.Financing.sources, Period, Programme), by = "ID_PROJECT")

# correct variable
unique(TypePartners$Co.Financing.sources)
TypePartners <- TypePartners %>% mutate(Co.Financing.sources = ifelse(Co.Financing.sources == "", NA, Co.Financing.sources))
TypePartnersCntr <- TypePartnersCntr %>% mutate(Co.Financing.sources = ifelse(Co.Financing.sources == "", NA, Co.Financing.sources))
unique(TypePartnersCntr$Co.Financing.sources)

ItandDe <- TypePartnersCntr %>% filter(Country == "Italy" | Country == "Germany")

ItandDeProgram<-as.data.frame(table(ItandDe$Country, ItandDe$Programme))

Top10ItDe <- ItandDeProgram %>% ungroup() %>%group_by(Var1)%>% top_n( 10)

ggplot(Top10ItDe)+
  geom_bar(aes(x = reorder(Var2, - Freq), y = Freq), stat = "identity")+ 
  coord_flip()+facet_wrap(~Var1, scales = "free")+
  labs(y = "Nombre de participations sur la période 2000-2019", x = "Nom des programmes", 
       title = "Top 10 des programmes rassemblant le plus de participations pour L'Allemagne et l'Italie",
       caption = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG 2019")

npartners <- TypePartners %>% group_by(TotalPartners) %>% summarise(Nproject = n())
npartnersperiod <- TypePartners %>% group_by(TotalPartners, Period) %>%summarise(Nproject = n())
nperiod <- table(TypePartners$TotalPartners, TypePartners$Period)
nperiod
15672/17671
# ad mean abline
mu <- TypePartners %>% group_by(Co.Financing.sources) %>% summarise(grp.mean = mean(TotalPartners))
p<-ggplot(TypePartners, aes(x=TotalPartners, color=Co.Financing.sources, fill = Co.Financing.sources )) +
   geom_density(alpha=.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Co.Financing.sources),
             linetype="dashed")+ scale_color_brewer(palette="Dark2") + scale_fill_brewer(palette="Dark2")+ theme_classic()
p


mu <- TypePartners %>% group_by(Period) %>% summarise(grp.mean = mean(TotalPartners))
p<-ggplot(TypePartners, aes(x=TotalPartners, color=Period, fill = Period)) +
  geom_density(alpha=0.4) +
  geom_vline(data=mu, aes(xintercept=grp.mean, color=Period),
             linetype="dashed")+ scale_color_brewer(palette="Dark2") + scale_fill_brewer(palette="Dark2")+ 
 theme_linedraw()+facet_wrap(~Period)
p

###### Explore textual info


## Translate
library(deeplr)
Projects <- Projects %>% mutate(Text = paste(Description.1,Description.2,  Expected.Results.1 , sep = ". "))

ProjectsSample <- Projects %>% sample_n(size = 100)
ProjectsTrad <- toEnglish2(ProjectsSample$Text, get_detect = T)
