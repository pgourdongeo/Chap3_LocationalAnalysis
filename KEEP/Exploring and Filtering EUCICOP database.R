
library(tidyverse)
library(skimr)
#Load data
setwd("~/BD_Keep_Interreg/KEEP")

Projects <- read.csv2("DataSource/ProjectsID.csv",stringsAsFactors = F)


Partners <- read.csv2( "DataSource/PartnersIDProj.csv", stringsAsFactors = F)



skim(Projects)

# 
unique(Projects$Period) 
unique(Partners$Legal.Status)

Partners <- Partners %>% mutate(Legal.Status = recode(Legal.Status, "public" = "Public", "private" =  "Private" ))

unique(Partners$Legal.Status)

TypePartners <- Partners %>% group_by(ID_PROJECT, Legal.Status) %>% summarise(nMemberStatus = n()) %>% 
  spread(key = Legal.Status, value = nMemberStatus)%>% mutate_all(funs(replace_na(.,0)))

TypePartners$TotalPartners <- rowSums(TypePartners[,sapply(TypePartners, is.numeric)])

summary(TypePartners$TotalPartners)
ggplot(TypePartners, aes(x = TotalPartners)) + geom_histogram(color="black", fill="white", binwidth=1)  +   
  geom_vline(aes(xintercept=mean(TotalPartners)), color="red", linetype="dashed", size=1)                               

TypePartners <- TypePartners %>% left_join(select(Projects, ID_PROJECT, Co.Financing.sources, Period), by = "ID_PROJECT")%>% distinct()

unique(TypePartners$Co.Financing.sources)

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
 theme_linedraw()
p

###### Explore textual info


## Translate
library(deeplr)
Projects <- Projects %>% mutate(Text = paste(Description.1,Description.2,  Expected.Results.1 , sep = ". "))

ProjectsSample <- Projects %>% sample_n(size = 100)
ProjectsTrad <- toEnglish2(ProjectsSample$Text, get_detect = T)
