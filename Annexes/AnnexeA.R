
##==========================================================================##         
#                                 ANNEXE A
#                          
# DESCRIPTION : Cartographie des différentes typologies
#
#
# PG, Aout 2021
##==========================================================================##         

# Working directory huma-num
# setwd("~/BD_Keep_Interreg/CITY")


# Library
library(tidyverse)

library(sf)
library(mapview)

library(tidylog)

library(skimr)



# Import data
sfEU <- st_read("KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("KEEP/AD/FDCARTE/rec_3035.geojson",  crs = 3035)

nutsUR <- st_read("OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) 

NUTS_CP_1420 <- st_read("KEEP/AD/SHP/NUTS2_StructuralFonds/NUTS2_CohesionPolicy_2014_2020.shp", crs = 3035) 
NUTS_EF_0613 <-st_read("KEEP/AD/SHP/NUTS2_StructuralFonds/NUTS2_EuropeanFunds_2006_2013.shp",  crs = 3035) 

TypoPolitics <- read.csv2("CountryInfo_PoliticalTypo.csv", stringsAsFactors = FALSE)


#### ==== MAPPING TYPOLOGIES OF LOCAL AUTHORITIES IN EUROPE ====

# Change Outside typologie as NA

TypoPolitics2 <- TypoPolitics %>% mutate_at(vars(11:15), na_if, "OutsideTypology")

# Deal with France and Norway

sfEU <- sfEU %>% mutate(ISO_A2 = case_when(NAME_EN == "Norway" ~ "NO", 
                                           NAME_EN == "France" ~ "FR",
                                           T ~ ISO_A2))

# Join typologies on local powers

EUlocalPower <- sfEU %>% left_join(select(TypoPolitics2, "ISO_A2" = iso_a2,LocGovType_HorizontalPwrRelation, LocGovType_VerticalPwrRelation, 
                                          LocGovType_PoliticalLeadership, LocGovTyp_EasternEurope, LocGovType_MunicipalAdmin))



##bounding box
bbrec <- st_bbox(rec)

## create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(st_bbox(rec)[3]-900000), c(st_bbox(rec)[3]-400000)),
                         Y = c(c(st_bbox(rec)[2]+200000), c(st_bbox(rec)[2]+200000)))

### LocGovType_HorizontalPwrRelation ###
HorizontalPower <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = EUlocalPower,
          aes(fill = LocGovType_HorizontalPwrRelation), colour = "ivory3", size = 0.4) +
  scale_fill_discrete(name = "Typologie horizontale sur les pouvoirs locaux\n(Mouritzen, Svara, 2002)",
                     na.value = "ivory4") +
  annotate("text", label = "Source : Mouritzen P.E., Svara J.H., 2002, Leadership at the Apex: Politicians and Administrators in Western Local Governments, University of Pittsburgh Press, 358 p. ",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.22, 0.6), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))


### display and save
pdf(file = "Annexes/OUT_A/LocGovType_HorizontalPwrRelation.pdf", width = 8.3, height = 5.8)
HorizontalPower
dev.off()



### LocGovType_VerticalPwrRelation ###
VerticalPower <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = EUlocalPower,
          aes(fill = LocGovType_VerticalPwrRelation), colour = "ivory3", size = 0.4) +
  scale_fill_discrete(name = "Typologie verticale sur les pouvoirs locaux\n(Hesse & Sharpe, 1991)",
                      na.value = "ivory4") +
  annotate("text", label = "Source : Hesse J.J., Sharpe L.J., 1991, « Local government in international perspective: some comparative observations », in Local government and urban affairs in international perspective, Nomos, p. 623.",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.22, 0.6), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))


### display and save
pdf(file = "Annexes/OUT_A/LocGovType_VerticalPwrRelation.pdf", width = 8.3, height = 5.8)
VerticalPower
dev.off()



### LocGovType_PoliticalLeadership ###
PoliticalLeadership <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = EUlocalPower,
          aes(fill = LocGovType_PoliticalLeadership), colour = "ivory3", size = 0.4) +
  scale_fill_discrete(name = "Typologie POLLEADER\n(Heinelt & Hlepas, 2006)",
                      na.value = "ivory4") +
  annotate("text", label = "Source : Heinelt, Hlepas, 2006, « Typologies of Local Government Systems »,\nin Bäck, Heinelt, Magnier, The European mayor: political leaders in the changing context of local democracy, Wiesbaden, Allemagne, VS Verlag, p.21‑42.",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.22, 0.6), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))


### display and save
pdf(file = "Annexes/OUT_A/LocGovType_PoliticalLeadership.pdf", width = 8.3, height = 5.8)
PoliticalLeadership
dev.off()


### LocGovType_EasternEurope ###
PoliticalEastern <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = EUlocalPower,
          aes(fill = LocGovTyp_EasternEurope), colour = "ivory3", size = 0.4) +
  scale_fill_discrete(name = "Typologie pouvoirs locaux Europe orientale\n(Swianiewicz, 2014)",
                      na.value = "ivory4") +
  annotate("text", label = "Source : Swianiewicz P., 2014, « An Empirical Typology of Local Government Systems in Eastern Europe », Local Government Studies, 40, 2, p.292‑311.",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.22, 0.6), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))


### display and save
pdf(file = "Annexes/OUT_A/ LocGovType_EasternEurope.pdf", width = 8.3, height = 5.8)
PoliticalEastern
dev.off()



### LocGovType_MunicipalAdmin ###
MunicipalAdmin <- ggplot() +
  geom_sf(data = sfEU, color = "ivory3", fill = "#f9e8d0", size = 0.4) +
  geom_sf(data = EUlocalPower,
          aes(fill = LocGovType_MunicipalAdmin), colour = "ivory3", size = 0.4) +
  scale_fill_discrete(name = "Typologie pouvoirs et administration locale\n(Kuhlmann & Wollmann, 2019)",
                      na.value = "ivory4") +
  annotate("text", label = "Source : Swianiewicz P., 2014, « An Empirical Typology of Local Government Systems in Eastern Europe », Local Government Studies, 40, 2, p.292‑311.",
           size = 2.2, hjust = 1,
           x = c(bbrec[3]), y = c(bbrec[2]-130000)) +
  geom_sf(data = rec, color = "ivory4", fill = NA) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bbrec[3]-800000), y = c(bbrec[2]+280000)) +
  coord_sf(crs = 3035, datum = NA,
           xlim = bbrec[c(1,3)],
           ylim = bbrec[c(2,4)]) +
  theme_void() +
  theme(legend.position = c(0.22, 0.6), 
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10))


### display and save
pdf(file = "Annexes/OUT_A/ LocGovType_MunicipalAdmin.pdf", width = 8.3, height = 5.8)
MunicipalAdmin
dev.off()
