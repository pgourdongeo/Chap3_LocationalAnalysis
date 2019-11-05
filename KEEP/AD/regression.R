###############################################################################
#                     EXPLORATION DE LA BD 'Project Partner' 
#
# DESCRIPTION : analyse bivariée nombre de participations et taille des umz 
# régression linéaire et carto des résidus
# PG, AD
# Octobre 2019
##############################################################################

## Working directory huma-num
#setwd("~/BD_Keep_Interreg/KEEP")

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
#library(ggpubr)
#library(GGally)


# Import data
load("AD/Keep_ClosedProject_Partner_corrected.RDS")

sfEU <- st_read("AD/FDCARTE/fdEurope_3035.geojson", crs = 3035) %>% 
  st_make_valid()

sfPartner <- st_as_sf(Partner, coords = c("lon", "lat"), crs = 4326) %>%
  st_sf(sf_column_name = "geometry") %>% 
  st_transform(crs = 3035)

rec <- st_read("AD/FDCARTE/rec_3035.geojson")

umz <- st_read("../TradeveShape/Agglo_Perimetre2001_Pop1961_2011.shp", crs = 3035)
#plot(st_geometry(umz))


# Intersect umz and participations
inter <- st_intersects(umz, sfPartner)

# Count points in polygons
umz <- st_sf(umz, 
             n = sapply(X = inter, FUN = length), 
             geometry = st_geometry(umz))

# 2284 / 3962 umz with no project :
# sum(umz$n == 0)
# 3507 umz with less than 11 projects
# sum(umz$n < 11)

# # rank
# umz <- umz %>% 
#   mutate(rank11 = row_number(desc(Pop2011)))
# 
# # display barplot with rank
# n <- ggplot(umz %>% dplyr::filter(n>=10), aes(x = rank11, y = n)) +
#   geom_point () +
#   theme_light() +
#   scale_y_continuous(trans = 'log10') +
#   # scale_x_continuous(trans = 'log10') +
#   labs(x = "rang umz (pop 2011)", y = "Nombre de participations")
# 
# n

# display barplot with log10
regUmz <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = Pop2011, y = n)) +
  geom_point () +
  theme_light() +
  scale_y_continuous(trans = 'log10') +
  scale_x_continuous(trans = 'log10') +
  labs(x = "Population 2011 des agglomérations UMZ (log10)", 
       y = "Nombre de participations aux projets Interreg \ndes entités localisées dans une UMZ (log10)") 
  #+ geom_smooth(method = 'lm')
regUmz

# ggscatter(umz, x = "Pop2011", y = "n", add = 'reg.line') +
#   stat_cor(label.y = 300) +
#   stat_regline_equation(label.y = 200)

# Estimer la regression linéaire
require(stats)
reg <- lm(log10(n) ~ log10(Pop2011) , data = umz %>% dplyr::filter(n > 10))
coeff <- coefficients(reg)
reg$coefficients

summary(reg)

# Equation de la droite de regression :
eq = paste0("y = 10^ ", round(reg$coefficients[1],2), " * x ^", round(reg$coefficients[2],2))  # On fait l'inverse de lg(y), y = 10^b * x^a

# Graphe
regUmz + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  #ggtitle(eq) + 
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 



summary(reg)
residuals(reg)

# library(purrr)
# rez <- reg %>% 
#   map("residuals") 

# add residuals and standart residuals
umz <- umz %>% 
  filter(n > 10)%>%
  mutate(rezStand = residuals(reg, type = "pearson")) %>% 
  ungroup()

sdRez <- sd(umz$rezStand)

# # Fonction pour identifier des outliers dans une distribution :
# is_outlier <- function(x) {
#   
#   return(x < quantile(x, 0.25) - (1.5 * IQR(x)) | x > quantile(x, 0.75) + 1.5 * IQR(x))
#   
# }
# 
# a <- umz %>% 
#   filter(n > 10) %>% 
#   st_drop_geometry() %>% 
#   select(rez)
# 
# 1.5*IQR(a$rez)
# quantile(a$rez, 0.25)-0.8361051
# quantile(a$rez, 0.75)+0.8361051
# 
# d <- ggplot(umz %>% dplyr::filter(n > 10), aes(x = rez)) +
#   geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
#   geom_density(alpha = .2, fill = "#FF6666") +
#   geom_vline(aes(xintercept = -1.079464),
#                  color = "blue", linetype = "dashed", size=1) +
#   geom_vline(aes(xintercept = 1.150149),
#              color = "blue", linetype = "dashed", size=1)

# # Ajout d'une variable Outlier au DF
# umz2 <- umz %>% 
#   group_by(n10 = n > 10) %>%
#   mutate(outlier_rez = ifelse(is_outlier(rez), 
#                               rez, 
#                               as.numeric(NA)))

# Fonction pour identifier des outliers dans une distribution :
is_outlier <- function(x) {
  
  return(x < -2 * sdRez | x > 2 * sdRez)
  
}

# Ajout d'une variable Outlier au DF
umz <- umz %>%
  mutate(outlier_rezStand = ifelse(is_outlier(rezStand), 
                                   rezStand, 
                                   as.numeric(NA)))

# add outliers name to the plot
pdf(file = "lm_umz.pdf",width = 8.3, height = 5.8, pagecentre =TRUE)
regUmz + 
  geom_label_repel(data = umz %>% filter(!is.na(outlier_rezStand)), 
                   aes(label = paste(Name, Country, sep = ", ")),
                   na.rm = TRUE, nudge_y = 0.05, color = "black") + 
  geom_abline(intercept = -0.3, slope = 0.34, color="red",
              linetype = "dashed", size = 1.5) +
  annotate(geom="text", x= 25000, y= 250, label= paste0(eq, "\nR2 = 0.33"),
           color="black") 
dev.off()


# FUNCTION - Display the residuals map
rezMap <- function(frame, bgmap, units, units2, var, titleLeg){
  
  #c(bottom, left, top, right)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  bb <- st_bbox(frame)
  # Plot
  plot(st_geometry(frame), border = NA, col = "#A6CAE0",
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#f0f0e9", border = "ivory3", lwd = 0.5, add = T)
  choroLayer(units, var = var, border = NA, breaks= bks, col= cols, 
             legend.pos = var, add = TRUE)
  #plot(st_geometry(units), col = NA, border = "ivory4", lwd = 0.05, add = T)
  
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
              sources = "sources :", 
              author = "PG, AD, 2019", 
              horiz = F,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = "bottomleft")
  
}

# # residuals 
# ## defines a set of breaks and colors 
# bks <- c(-1.26, -1, -0.5, 0.5, 1, 1.52)
# cols <- c("#1A7832","#AFD4A0", "#e8deae", carto.pal("wine.pal",2))
# 
# ## Plot the residuals map
# rezMap(frame = rec, 
#        bgmap = sfEU, 
#        units = umz %>% filter(n > 10), 
#        var = "rez",
#        titleLeg = "résidus\n(umz avec moins de 11 projets = 88%)")

# residuals standart
## defines a set of breaks and colors 
#bks <- c(-1, -0.5, 0, 0.5, 1)
#cols <- carto.pal("green.pal",2 ,"wine.pal",2)
bks <- c(min(umz$rezStand), -2 * sdRez, -1 * sdRez, 1 * sdRez, 2 * sdRez, max(umz$rezStand))
cols <- c("#1A7832", "#AFD4A0", "#f6f5c5", carto.pal("wine.pal",2))

## Plot the residuals map
#pdf(file = "rezS.pdf",width = 8.3, height = 5.8, pagecentre =FALSE)
rezMap(frame = rec, 
       bgmap = sfEU, 
       units = umz %>% filter(n > 10), 
       var = "rezStand",
       titleLeg = "résidus standardisés\n")
dev.off()


