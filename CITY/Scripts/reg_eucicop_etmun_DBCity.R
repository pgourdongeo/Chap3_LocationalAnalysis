
##==========================================================================##         
#                                 BD city - régression linéaire
#                          
# DESCRIPTION : 
#
#
# PG, AD, avril 2020
##==========================================================================##         

# Working directory huma-num
# setwd("~/BD_Keep_Interreg/CITY")

setwd("~/git/Chap3_LocationalAnalysis/CITY")
options(scipen = 999)


# library
library(skimr)
library(tidylog)
library(tidyverse)
library(readr)
library(sf)
library(mapview)


# load data
city <- readRDS("Data/DBCity.rds")
sfCity <- st_as_sf(city, coords = c("lng_GN", "lat_GN"), crs = 4326) %>% 
  st_transform(crs = 3035)

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", 
               stringsAsFactors = FALSE, crs = 3035)
fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp", 
               stringsAsFactors = FALSE) %>% 
  st_transform(crs = 3035)

lau <- readRDS("Data/AdminDelimPop0611.RDS")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", 
                stringsAsFactors = FALSE, crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")


# ==== Spatial join with LAU2, UMZ and FUA ====

sfCity <- st_join(sfCity, 
                select(lau, CODE_LAU = Code_2, NAME_LAU = Name_2, PopAdmin06, PopAdmin11),
                join = st_intersects)

sfCity <- st_join(sfCity,
                  select(umz, ID_UMZ, NAME_UMZ = Name, POPUMZ11 = Pop2011),
                  join = st_intersects)

fua <- fua %>% filter(URAU_CATG == "L")
sfCity <- st_join(sfCity,
                  select(fua, ID_FUA = URAU_ID, NAME_FUA = URAU_NAME, POPFUA06 = URAU_POPL),
                  join = st_intersects)



## ==== Spatial extent for linear regression ====


## filter cities in Europe frame
#sfCityEur <- st_intersection(sfCity, rec)

# filter cities in UE 
iso <- c("IE", "GB", "PT", "ES", "FR", "BE", "NL", "LU", "DE", "DK",
         "AT", "IT", "GR", "SI", "HR", "CZ", "PL", "SK", "HU", "BG", 
         "RO", "EE", "LT", "LV", "CY", "FI", "SE", "MT")
sfCityEur <- sfCity %>% filter(countryCode %in% iso)




## ==== linear regression ====

# Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2.5 * sdRez | x > 2.5 * sdRez)
  
}

sfCityEur <-sfCityEur %>% 
  mutate(TOT_PART = members_etmun + participations_eucicop)

sfCityEur_filter <- sfCityEur %>% filter(PopAdmin11 > 5000 | TOT_PART > 10)

## regression
reg <- lm(participations_eucicop ~ members_etmun , data = sfCityEur_filter)
summary(reg)


## Equation de la droite de regression :
eq = paste0("y = ", round(reg$coefficients[2],2), " * x + ", round(reg$coefficients[1],2))

## Residuals
### add residuals and standart residuals to df
sfCityEur_filter <- sfCityEur_filter %>% 
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(sfCityEur_filter$rezStand)

## Outliers
### Ajout d'une variable Outlier au DF
sfCityEur_filter <- sfCityEur_filter %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## ggplot
require(ggrepel)

regEucEtm <- ggplot(sfCityEur_filter, aes(x = members_etmun, y = participations_eucicop)) +
  geom_point () +
  theme_light() +
  labs(x = "Nombre d'adhésions aux associations ETMUN", 
       y = "Nombre de participations aux projets Interreg (EUCICOP)") + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  geom_label_repel(data = sfCityEur_filter %>% filter(rezStand > 300), 
                   aes(label = paste(asciiName, countryCode, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) +
  annotate(geom = "label", x = 30, y = 450, label= paste0(eq, "\nR2 = ", 
          round(summary(reg)$r.squared, 2)), hjust = 0, fill = "#E69F00", size = 3) +
  labs(caption = "Sources : ETMUN 2019 ; Tradeve 2015 ; EUCICOP 2019 / PG, AD, 2020", size = 2.5) 

pdf(file = "OUT/reg_eucicope_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
regEucEtm 
dev.off()



# ==== residuals map ====

## Function
rezMap_propChoro <- function(frame = rec, bgmap = sfEU, units, var, myVal, var2, 
                             title1, labFilter, source) {
  
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5)
  propSymbolsChoroLayer(units, 
                        var = var, inches = 0.3, border = "grey60", lwd = 0.5, symbols = "square",
                        var2 = var2, breaks = bks, col = cols,
                        legend.var.pos = NA, legend.var2.pos = NA)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, add = TRUE)
  
  # Add legend
  legendSquaresSymbols(pos = c(1000000, 4400000),
                       cex = 1,
                       var = myVal,
                       inches = 0.3, border = "grey60", lwd = 0.5, col = NA,
                       title.txt = title1, title.cex = 0.8, values.cex = 0.6)
  
  legendChoro(pos = c(1000000, 3000000), 
              cex = 0.9,
              title.txt = "Résidus Standardisés*", title.cex = 0.8, values.cex = 0.6,
              breaks = bks, col = cols, values.rnd = 2, nodata = F, 
              border = "grey60")
  
  # display labels 
  labelLayer(x = units %>% top_n(TOT_PART, n = 20), txt = "asciiName", overlap = FALSE,
             col = "#4d4d4d", show.lines = TRUE, cex = 0.5, pos = 4, offset = 0.2)
  
  # Add an explanation text
  labels <-  paste("*Résidus de la régression :\n", eq, 
                 ",\ndiscrétisés selon la moyenne\ndes résidus et 2 écarts-types", 
                 labFilter, sep = "")
  text(x = 1000000, y = 2300000, labels = labels, cex = 0.65, adj = 0)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = source,
        side = 1, 
        line = -1.2, 
        adj = 0.935,
        cex = 0.50)
  
}



## ----~ with propportional pop ----
### defines a set of breaks and colors 
require(cartography)

ol <- sfCityEur_filter %>% filter(rezStand < -2*sdRez | rezStand > 2*sdRez | PopAdmin11 > 1000000)

bks <- c(min(ol$rezStand), -6 * sdRez, -4 * sdRez, -2 * sdRez, 
         2 * sdRez, 4 * sdRez, 6 * sdRez, max(ol$rezStand))
cols <- carto.pal("green.pal",3, "wine.pal",3, middle = TRUE)


### Plot 
pdf(file = "OUT/rez1_eucicope_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = ol,
                 var = "PopAdmin11", 
                 myVal = c(10000, 500000, 3000000, 9000000),
                 var2 = "rezStand",
                 title1 = "Population administrative des villes en 2011", 
                 labFilter = "\n\nNB : Les villes comptant moins\nde 1 million d'habitants\nou sans résidus extrêmes\nn'apparaissent pas sur la carte.\nNom des villes = Top 20\nde la somme des participations\nEucicop/Etmun", 
                 source = "Sources : EUCICOP 2019 ; ETMUN 2019 ; GEOSTAT LAU 2017, POPGRID 2011/ PG, AD, 2020")
dev.off()


## Plot with propportional eucicop members
### defines a set of breaks and colors 
require(cartography)

# ol <- sfCityEur_filter %>% filter(rezStand < -2*sdRez | rezStand > 2*sdRez | PopAdmin11 > 1000000)
# skim(ol$PopAdmin11)
# 
# bks <- c(min(ol$rezStand), -6 * sdRez, -4 * sdRez, -2 * sdRez,
#          2 * sdRez, 4 * sdRez, 6 * sdRez, max(ol$rezStand))
# cols <- carto.pal("green.pal",3, "wine.pal",3, middle = TRUE)


bks <- c(min(sfCityEur_filter$rezStand), -6 * sdRez, -4 * sdRez, -2 * sdRez,
         2 * sdRez, 4 * sdRez, 6 * sdRez, max(sfCityEur_filter$rezStand))
cols <- carto.pal("green.pal",3, "wine.pal",3, middle = TRUE)

partEuci <- sfCityEur_filter %>% filter(participations_eucicop > 0)
skim(partEuci$participations_eucicop)

### Plot 
pdf(file = "OUT/rez3_eucicop_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = sfCityEur_filter %>%  filter(PopAdmin11 > 10000),
                 var = "participations_eucicop", 
                 myVal = c(1, 50, 300, 701),
                 var2 = "rezStand",
                 title1 = "Nombre de participations EUCICOP", 
                 labFilter = "\n\nNB : Les villes comptant moins\nde 1 million d'habitants\nou sans résidus extrêmes\nn'apparaissent pas sur la carte", 
                 source = "Sources : EUCICOP 2019 ; ETMUN 2019 ; GEOSTAT LAU 2017, POPGRID 2011/ PG, AD, 2020")
dev.off()


## test
ol <- sfCityEur_filter %>% filter(rezStand < -2*sdRez | rezStand > 2*sdRez)

bks <- c(min(ol$rezStand), -6 * sdRez, -4 * sdRez, -2 * sdRez,
         2 * sdRez, 4 * sdRez, 6 * sdRez, max(ol$rezStand))
cols <- carto.pal("green.pal",3, "wine.pal",3, middle = TRUE)

# partEuci <- sfCityEur %>% filter(participations_eucicop > 0)
# skim(partEuci$participations_eucicop)

### Plot 
pdf(file = "OUT/rez4_eucicop_etmun.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = ol,
                 var = "TOT_PART", 
                 myVal = c(5, 50, 300, 721),
                 var2 = "rezStand",
                 title1 = "Nombre de participations EUCICOP", 
                 labFilter = "\n\nNB : Les villes comptant moins\nde 1 million d'habitants\nou sans résidus extrêmes\nn'apparaissent pas sur la carte", 
                 source = "Sources : EUCICOP 2019 ; ETMUN 2019 ; GEOSTAT LAU 2017, POPGRID 2011/ PG, AD, 2020")
dev.off()








