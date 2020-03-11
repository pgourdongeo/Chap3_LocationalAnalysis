
##==========================================================================##         
##            ANALYSE BIVARIEE : participations ~ taille des agglo          ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base Eucicop/keep / régression linéaire et                 ##
##               cartographie des résidus                                   ##
##                                                                          ##
## PG, AD, Octobre 2019                                                     ##
##==========================================================================##

# CONTENTS
## 1. partcipation ~ UMZ 
## 2. partcipation ~ FUA 
## 3. - Fig. 3.13


# Working directory huma-num
setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)

# Library
library(cartography)
library(dplyr)
library(sf)
library(tidylog)
library(skimr)
library(lwgeom)
library(ggplot2)
library(ggrepel)
library(GGally)



# Import data

sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)

fua <- st_read("../OtherGeometry/ShpUrbanAudit2012_Pop2006/URAU_2012_RG.shp") %>% 
  st_transform(crs = 3035)

## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")




# ================ Functions ================ 

# FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, var, source, titleLeg){
  
  par(mar = c(0, 0, 0, 0)) 
  bb <- st_bbox(frame)
  
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#f0f0e9", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = var, border = NA, breaks= bks, col= cols, 
             legend.pos = var, add = TRUE)
  
  # Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8,
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = F, 
              values.rnd = 2, 
              col = cols)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = source, 
              author = "PG, AD, 2019", 
              horiz = F,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}


rezMap_propChoro <- function(frame = rec, bgmap = sfEU, units, var, myVal, var2, 
                             title1, title2, labels, source) {
  
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5)
  propSymbolsChoroLayer(units, 
                        var = var, inches = 0.3, border = "grey60", lwd = 0.5, symbols = "square",
                        var2 = var2, breaks = bks, col = cols,
                        legend.var.pos = NA, legend.var2.pos = NA)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, add = TRUE)
  
  # Add legend
  legendSquaresSymbols(pos = c(1000000, 4200000),
                       cex = 1,
                       var = myVal,
                       inches = 0.3, border = "grey60", lwd = 0.5, col = NA,
                       title.txt = title1, title.cex = 0.8, values.cex = 0.6)
  
  legendChoro(pos = c(1000000, 3000000), 
              cex = 0.9,
              title.txt = title2, title.cex = 0.8, values.cex = 0.7,
              breaks = bks, col = cols, values.rnd = 2, nodata = F)
  
  
  # Add an explanation text
  text(x = 1000000, y = 2600000, labels = labels, cex = 0.7, adj = 0)
  
  # Add a layout
  layoutLayer(title = "", 
              sources = source, 
              author = "PG, AD, 2019", 
              horiz = FALSE,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}

# Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2 * sdRez | x > 2 * sdRez)
  
}




# ==== 1. partcipation ~ UMZ ==== 
#NO CORRELATION

## Count participations in umz
### Intersect umz and participations
. <- st_intersects(umz, sfParticipations_snap)
### Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = ., FUN = length), 
             geometry = st_geometry(umz))

# 1744 / 3962 umz with no project :
# sum(umz$n == 0)
# 3144 umz with less than 11 projects
# sum(umz$n < 11)


# Filter value
thrshld <- 10
# Test R2 with threshold value
R2 <- list()

for(i in seq(1,20, 1)){
  reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > i))
  R2[[i]] <- summary(reg)$r.squared
}


## display graph
### with log10


regUmz <- ggplot(umz %>% dplyr::filter(n > thrshld), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population 2011 des agglomérations UMZ (log10)", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ (log10)") 
regUmz

### Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > thrshld))
summary(reg)

### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regUmz + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 25000, y = 1000, label = paste0(eq, "\nR2 = ", 
                                          round(summary(reg)$r.squared, 2))) 


## Residuals
### add residuals and standart residuals to df
umz <- umz %>% 
  filter(n > thrshld)%>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(umz$rezStand)

## Outliers
### Ajout d'une variable Outlier au DF
umz <- umz %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## Plot with outliers and save pdf - fig. 3.12
pdf(file = "AD/OUT/lm_umz.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regUmz + 
  geom_label_repel(data = umz %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(Name, Country, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) + 
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "label", x = 3000000, y = 50, label= paste0(eq, "\nR2 = ", 
              round(summary(reg)$r.squared, 2)), hjust = 0, fill = "#E69F00", size = 3) +
  labs(caption = "Sources : EUCICOP 2019 ; Tradeve 2015", size = 3)
dev.off()



## residuals map (residuals standart)
### defines a set of breaks and colors 
bks <- c(min(umz$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(umz$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

skim(umz$Pop2011)

### Plot and save
#pdf(file = "AD/OUT/rez_umz.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = umz %>% filter(n > thrshld),
                 var = "Pop2011", 
                 myVal = c(8000, 1000000, 5000000, 10000000),
                 var2 = "rezStand",
                 title1 = "Population des UMZ en 2011", 
                 title2 = "Résidus Standardisés*",
                 labels = paste("*Résidus de la régression :\n", eq,",\ndiscrétisés selon la moyenne\ndes résidus (=0) et 1 écart-type", sep = ""),
                 source = "Sources :")
dev.off()


### Plot and save ---OLD
#pdf(file = "AD/OUT/rez_umz.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
# rezMap(frame = rec, 
#        bgmap = sfEU, 
#        units = umz %>% filter(n > 10), 
#        var = "rezStand",
#        source = "Sources :",
#        titleLeg = "résidus standardisés\n")
# dev.off()




# ==== 2. partcipation ~ FUA  ==== 
#No CORRELATION
## select only LUZ 
fua <- fua %>% filter(URAU_CATG == "L")

## Count participations in FUA
### Intersect fua and participations
. <- st_intersects(fua, sfParticipations_snap)
### Count points in polygons
fua <- st_sf(fua, 
             n = sapply(X = ., FUN = length), 
             geometry = st_geometry(fua))

## display graph
### with log10
regFua <- ggplot(fua %>% dplyr::filter(n > 10), aes(x = URAU_POPL, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population des aires urbaines fonctionnelles (FUA) en 2006\n(log10)", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une FUA (log10)") 
regFua


### Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(URAU_POPL) , data = fua %>% dplyr::filter(n > 10))
summary(reg)
### Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

### Add line et equation
regFua + 
  geom_abline(intercept = -1.17, slope = 0.54, color = "#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 80000, y = 1000, label= paste0(eq, "\nR2 = 0.25")) 

## Residuals
### add residuals and standart residuals to df
fua <- fua %>% 
  filter(n > 10) %>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(fua$rezStand)

## Outliers
### Ajout d'une variable Outlier au DF
fua <- fua %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

## Plot with outliers and save pdf
#pdf(file = "AD/OUT/lm_fua.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regFua + 
  geom_label_repel(data = fua %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(URAU_NAME, CNTR_CODE, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black") + 
  geom_abline(intercept = - 1.17, slope = 0.54, color = "#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 50000, y = 1000, label = paste0(eq, "\nR2 = 0.25"), hjust = 0) 
dev.off()



## residuals map (residuals standart)
### defines a set of breaks and colors 
bks <- c(min(fua$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(fua$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

skim(fua$URAU_POPL)

### Plot and save
pdf(file = "AD/OUT/rez_fua_test.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap_propChoro(units = fua %>% filter(n > 10),
                 var = "URAU_POPL", 
                 myVal = c(50000, 2000000, 5000000, 11000000),
                 var2 = "rezStand",
                 title1 = "Population des aires urbaines en 2006", 
                 title2 = "Résidus Standardisés*",
                 labels = paste("*Résidus de la régression :\n", eq,",\ndiscrétisés selon la moyenne\ndes résidus (=0) et 1 écart-type", sep = ""),
                 source = "Sources :")
dev.off()


### Plot and save  ---- OLD
#pdf(file = "AD/OUT/rez_fua.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
# rezMap(frame = rec, 
#        bgmap = sfEU, 
#        units = fua %>% filter(n > 10), 
#        var = "rezStand",
#        source = "Sources :",
#        titleLeg = "résidus standardisés\n")
# dev.off()



# ==== 3. partcipation ~ FUA & UMZ . Figure 3.13 ==== 
# Make two geom points to show the absence of structure
## Filter Paris and London outliers
umz <- umz %>% mutate(Name = recode(Name, "Helsinki = Helsingfors" = "Helsinki", 
                                    "Essen // Duisburg // Dortmund // Bochum // Gelsen*" = "Essen/Dortmund",
                                    "Lille // Roubaix // Tourcoing" = "Lille"))

regUmz <- ggplot(umz %>% filter(Pop2011< 9000000), aes(x = Pop2011, y = n)) +
  geom_point () +
  geom_label_repel(data = umz %>% filter(Pop2011< 9000000)%>%filter(Pop2011> 3500000 | n>500) , 
                   aes(x = Pop2011, y = n, label = Name),  na.rm = TRUE, nudge_y = 0.05, color = "black", size = 4)+
  theme( axis.title.x = element_text(size = 14),
               axis.title.y = element_text(size = 14)) +
  labs(x = "Population 2011 des agglomérations UMZ", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ") 

regFua <- ggplot(fua %>% filter(URAU_POPL< 10000000), aes(x = URAU_POPL, y = n)) +
  geom_point () +
  geom_label_repel(data = fua %>% filter(URAU_POPL< 10000000)%>%filter(URAU_POPL> 4500000 | n>510) , 
                   aes(x = URAU_POPL, y = n, label = URAU_NAME),  na.rm = TRUE, nudge_y = 0.05, color = "black", size = 4)+
  theme( axis.title.x = element_text(size = 14),
         axis.title.y = element_text(size = 14)) +
  labs(x = "Population des aires urbaines (FUA) en 2006", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une FUA") +
  labs(caption = "Sources : EUCICOP 2019 ; Tradeve 2015 ; URBAN AUDIT 2012")

library(cowplot)
# Make a grid with the two plots (increase export dimensions to get all the plot)
duogg<-plot_grid(regUmz, regFua)
ggsave2(duogg, filename = "AD/OUT/lm_umz_fua.pdf",
        width = 10, height = 7, units = "in", dpi = 600)
