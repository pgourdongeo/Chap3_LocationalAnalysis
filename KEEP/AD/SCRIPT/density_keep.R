###############################################################################
#         CARTOGRAPHIE DES DENSITES de participations aux projets de l'UE
#                          à partir de la BD KEEP
#
# DESCRIPTION : construction d'un carroyage sur l'europe élargie, comptage des 
# participations aux projets de l'UE par carreau, cartographie des densités de 
# participations sur plusieurs périodes, cartographie des densités des participations 
# des partenaires leader sur toute la période 2000-2018. Cartographie des densités par nuts
# et graphique des participations par type de nuts (urbain/rural)
#
# PG, AD, Octobre 2019
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
library(readr)
library(RColorBrewer)
library(mapview)
#library(ggpubr)
#library(GGally)


# Import data
participations <- readRDS("Data/Participations_All_Eucicop.RDS")
partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")
projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")

sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()

## data with snaped points 
sfParticipations_snap <- readRDS("sfParticipations_snap.RDS")

# Functions
#================================================

## Build a regular grid and count points in each cell
pt_in_grid <- function(feat, adm, cellsize){
  
  ptgrid <- list()
  
  # Create a regular grid (adm bbox)
  grid <- st_make_grid(x = adm, cellsize = cellsize, what = "polygons")
  
  # Keep only cells that intersect adm
  . <- st_intersects(grid, adm)
  grid <- grid[sapply(X = ., FUN = length)>0]
  
  # # cut cells in the borders
  # ptgrid[["grid"]] <- st_intersection(grid, adm)
  grid <- st_intersection(grid, adm)

  # Count pts in grid
  . <- st_intersects(grid, feat)
  grid <- st_sf(n = sapply(X = ., FUN = length), grid)
  
  # cut cells in the borders
  ptgrid[["grid"]] <- grid 
  
  # remove null values
  ptgrid[["grid0"]] <- grid %>% 
    filter(n != 0)
  
  return(ptgrid)
}

## Plot a gridded map
plot_grid <- function(grid, adm, frame, sources, titleLeg){
  
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.5, add = T)

  ## Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8, 
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
  
  # Add a layout
  layoutLayer(title = "",
    sources = sources,
    author = "PG, AD, 2019",
    horiz = F,
    col = NA,
    frame = F,
    scale = 500,
    posscale = c(6500000, 1000000)
  )
  
}

## Plot 3 maps with a unique legend
plot_grids <- function(grid1, grid2, grid3, 
                       title1, title2, title3,
                       adm, frame, sources, titleLeg){
  
  bb <- st_bbox(frame)
  
  ## plot 3 maps on a single figure 
  par(mar = c(0, 0, 0, 0), mfrow = c(2, 2), ps=15)
  
  ## plot1
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
  # Add title
  mtext(text = title1,
        font = 2,
        side = 3, 
        line = -1, 
        adj = 0,
        cex =0.6)
  
  ## Add legend
  legendChoro(pos = c(1000000, 2500000),
              title.cex = 0.65,
              values.cex = 0.55,
              title.txt = titleLeg,
              breaks = bks,
              nodata = FALSE,
              values.rnd = 0,
              col = cols)
  
  
  ## plot2
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid2, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
  # Add title
  mtext(text = title2,
        font = 2,
        side = 3,
        line = -1,
        adj = 0,
        cex =0.6)
  
  
  ## plot3
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  choroLayer(grid3, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n", add = T)
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.1, add = T)
  
  # Add title
  mtext(text = title3,
        font = 2,
        side = 3,
        line = -1,
        adj = 0,
        cex =0.6)
  
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1, 
        adj = 0,
        cex =0.35)
  
}

## plot a points map
plot_points <- function(frame, adm, sf, txtLeg, source){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0))
  
  # Plot the map
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)], add = TRUE)
  
  # Add a legend
  legend(x = 1000000,
         y = 4500000,
         legend = txtLeg, 
         bty = "n",
         cex = 0.8)
  
  # Add a layout
  layoutLayer(title = "",
              sources = source,
              author = "PG, AD, 2019",
              horiz = FALSE,
              col = NA,
              frame = F,
              scale = 500,
              posscale = c(6500000, 1000000)
  )
}

## Plot a choro map
dens_map <- function(frame, bgmap, sf, titleLeg, sources){
  
  # set the margins
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(frame), border = NA, lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#E3DEBF", border = "ivory3", lwd = 0.5, add = TRUE)
  choroLayer(sf, var = "density", border = NA, breaks= bks, col= cols, 
             legend.pos = "density", add = TRUE)
  plot(st_geometry(sf), col = NA, border = "ivory4", lwd = 0.1, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
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
              sources = sources, 
              author = "PG, AD, 2019", 
              horiz = FALSE,
              col = NA, 
              frame = F, 
              scale = 500, 
              posscale = c(6500000, 1000000))
  
}


# Map participations/cell 2000-2018 - all partners
#================================================

## 50 km cells
europegrided <- pt_in_grid(feat = sfParticipations_snap, adm = sfEU, cellsize = 50000)

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Plot and save pdf
pdf(file = "AD/OUT/europeGrid_eucicopall.pdf",width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ", 
          titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2\n(discrétisation en progression géométrique)")
dev.off()

## PCT 0 participation

summary(europegrided[[1]]$n)
sum(europegrided[[1]]$n)
skim(europegrided[[1]])
nrow(europegrided[[1]])
nrow(europegrided[[1]][europegrided[[1]]$n == 0,])/ nrow(europegrided[[1]]) *100



# Maps participations/cell 2000-2006, 2007-2013 et 2014-2020
#================================================


## defines a unique set of breaks for all maps (same legend as the 2000-2018 map)
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))


## prepare grids
europegrided1 <- pt_in_grid(feat = sfParticipations_snap %>% filter(Period == "2000-2006"), 
                            adm = sfEU, cellsize = 50000)
europegrided2 <- pt_in_grid(feat = sfParticipations_snap %>% filter(Period == "2007-2013"), 
                            adm = sfEU, cellsize = 50000)
europegrided3 <- pt_in_grid(feat = sfParticipations_snap %>% filter(Period == "2014-2020"), 
                            adm = sfEU, cellsize = 50000)

## to do : % empty cells
# 7266 carreaux
hist(europegrided1[[1]]$n)
dplyr::filter(europegrided1[[1]], n == 0)
skim(europegrided1[[1]])
skim(europegrided2[[1]])
skim(europegrided3[[1]])
sum(europegrided1[[1]]$n) + sum(europegrided2[[1]]$n) + sum(europegrided3[[1]]$n)

nrow(europegrided1[[1]][europegrided1[[1]]$n == 0,])/ nrow(europegrided1[[1]]) *100
nrow(europegrided2[[1]][europegrided2[[1]]$n == 0,])/ nrow(europegrided2[[1]]) *100
nrow(europegrided3[[1]][europegrided3[[1]]$n == 0,])/ nrow(europegrided3[[1]]) *100

## display maps and save pdf
pdf(file = "AD/OUT/europeGridPeriod_eucicopall.pdf", width = 8.3, height = 5.8)
plot_grids(grid1 = europegrided1[[1]], 
           grid2 = europegrided2[[1]],
           grid3 = europegrided3[[1]],
           title1 = "2000-2006",
           title2 = "2007-2013",
           title3 = "2014-2020",
           adm = sfEU,
           frame = rec,
           titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2\n(progression géométrique)",
           sources = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019")
dev.off()


# Barplots numbers of participations by country 
#================================================

## data with snaped points 
sfParticipations_snap <- readRDS("sfParticipations_snap.RDS")

## Add ISO to europe shape
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
sfEU <- left_join(select(sfEU, ID, NAME_EN, UE28), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO_SF = ISO_A2), 
                   by = "NAME_EN")


## Count participations/coutry/period
dfPartPeriodCount <- sfParticipations_snap %>% 
  st_intersection(., select(sfEU, NAME_EN, ISO_SF, UE28)) %>%
  group_by(ISO_SF) %>% 
  mutate(nbp_c = sum(length(ID_PARTICIPATION))) %>% 
  group_by(Period, ISO_SF) %>% 
  mutate(nbp_cp = sum(length(ID_PARTICIPATION))) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  select(-geometry)

## Prepare df 
noEU <-  c("NO", "BA", "IS", "RKS", "LI", "ME", "MK", "RS", "CH", "TR")

dfpartCntr <- dfPartPeriodCount %>% 
  group_by(Period, ISO_SF) %>% 
  slice(1) %>% 
  filter(UE28 == TRUE | ISO_SF %in% noEU) %>% 
  select(NAME_EN, ISO_SF, UE28, nbp_c, Period, nbp_cp)

sum(dfpartCntr$nbp_cp)

### UE28
dfpartCntrUE <- dfpartCntr %>% filter(., UE28 == TRUE) %>% filter(!duplicated(ISO_SF))
sum(dfpartCntrUE$nbp_cp)

## Plot participation/country
partCntr <- ggplot(data = dfpartCntrUE, 
                   aes(x = reorder(NAME_EN, -nbp_c), y = nbp_c)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = nbp_c), 
            position = position_dodge(0.9), 
            vjust = 1.6, color = "white", size = 2) +
  labs(x = "UE28",
       y = "Nombre de participations sur la période 2000-2019") +
  theme_light() +
  labs(caption = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019") +
  theme(legend.position = c(0.6, 0.8),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.caption = element_text(size = 6)) 

## display end save
pdf(file = "AD/OUT/particip_country_eucicopall.pdf", width = 8.3, height = 5.8)
partCntr
dev.off()


## faceting participation/country/period
partCntrP <- ggplot(data = na.omit(dfpartCntrUE), 
       aes(x = reorder(NAME_EN, -nbp_c), y = nbp_cp)) +
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  geom_text(aes(label = nbp_cp), 
            position = position_dodge(0.9), 
            hjust = 1.4, vjust = 0.4, angle = 90, color = "white", size = 2.4) +
  labs(x = "UE28",
       y = "Nombre de participations") +
  facet_wrap(~Period) +
  theme_light() +
  labs(caption = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.25, size = 5.5),
        axis.text.y = element_text(size = 6),
        plot.caption = element_text(size = 6)) 

## display end save
pdf(file = "AD/OUT/particip_country_Period_eucicopall.pdf", width = 8.3, height = 5.8)
partCntrP
dev.off()



# map lead partner density
#================================================

sfParticipations <- readRDS("sfParticipations_full.RDS")

## Display points and save
pdf(file = "AD/OUT/densityLead_eucicopall.pdf", width = 8.3, height = 5.8)
plot_points(frame = rec,
            adm = sfEU,
            sf = sfParticipations %>% filter(Lead.Partner == "Yes"),
            txtLeg = "Chaque point représente\nune participation d'un lead partner\naux projets de l'UE",
            source = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019")
dev.off()


## Display density cells for lead partners only
### 50 km cells
europegridedL <- pt_in_grid(feat = sfParticipations_snap %>% filter(Lead.Partner == "Yes"), 
                           adm = sfEU, cellsize = 50000)
skim(europegridedL[[1]])
sum(europegridedL[[1]]$n)
### defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegridedL[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

### Plot and save pdf
pdf(file = "AD/OUT/europeGrid_lead_eucicopall.pdf",width = 8.3, height = 5.8)
plot_grid(grid = europegridedL[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019", 
          titleLeg = "Nombre de participations des lead partners\naux projets de l'UE\npar carreau de 2 500 km2\n(discrétisation en progression géométrique)")
dev.off()




# density of participations by nuts
#================================================

## Prepare data
### Intersect nuts and participations
inter <- st_intersects(nutsUR, sfParticipations_snap)
inter2 <- st_intersects(nutsUR, sfParticipations_snap %>% filter(Lead.Partner == "Yes"))
### Count points in polygons
nutsUR <- st_sf(nutsUR, 
                n = sapply(X = inter, FUN = length), 
                nL = sapply(X = inter2, FUN = length),
                geometry = st_geometry(nutsUR))

## Add density to df : nb of participations for 10 000 inhabitants
nutsUR <- nutsUR %>% 
  mutate(density = n / Pop_t_2001 * 10000)

## Display map
### defines a set of breaks and colors
myvar <- nutsUR %>% filter(density != 0) %>% select(density) 
bks <- c(0, getBreaks(v =  myvar$density, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

### Plot and save
pdf(file = "AD/OUT/density_nutsUR_eucicopall.pdf",width = 8.3, height = 5.8)
dens_map(frame = rec, 
        bgmap = sfEU, 
        sf = nutsUR, 
        titleLeg = "Nombre de participations\naux projets de l'UE\npour 10 000 habitants et par NUTS\n(discrétisation en progression géométrique)",
        sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ; ESPON DB 2013")
dev.off()



# Barplots participations/type of nuts
#================================================

# average numbers of participations by type of nuts
nutsUR <- nutsUR %>% 
  group_by(Typo7) %>% 
  mutate(nbm = mean(n),
         nbmL = mean(nL)) %>% 
  ungroup()

bibi <- data.frame(Typo7 = unique(nutsUR$Typo7), 
                   nbm = unique(nutsUR$nbm),
                   Lead = "Ensemble des partenaires")
bibi2 <- data.frame(Typo7 = unique(nutsUR$Typo7), 
                   nbm = unique(nutsUR$nbmL),
                   Lead = "Partenaires organisateurs uniquement")
bibi <- rbind(bibi, bibi2)

# create barplots
projNuts <- ggplot(data = bibi, aes(x = Typo7, y = nbm, fill = Lead)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(nbm)), position = position_dodge(0.9), vjust = 1.6, color = "white") +
  labs(x = "Types de NUTS",
       y = "Nombre moyen de participations sur la période 2000-2019") +
  scale_fill_manual(values=c("#999999", "#E69F00")) +
  theme_light() +
  labs(caption = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ; ESPON DB 2013\nPG, AD, 2019") +
  theme(legend.position = c(0.6, 0.8), legend.title = element_blank(),     
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        plot.caption = element_text(size = 6)) 

# display end save
pdf(file = "AD/OUT/particip_nutsUR_eucicopall.pdf", width = 8.3, height = 5.8)
projNuts
dev.off()




