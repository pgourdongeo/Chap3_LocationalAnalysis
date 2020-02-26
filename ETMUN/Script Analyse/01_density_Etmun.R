
##==========================================================================##         
##                         CARTOGRAPHIE DES DENSITES                        ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN / réalisations carto-graphiques                 ##
##               du chapitre 3 (carroyage, dot plot, bar plot)              ##
##                                                                          ##
## PG, AD, Octobre 2019                                                     ##
##==========================================================================##

# CONTENTS
# 1. Mapping: adhesions/cell 2019 - Fig. 3.13
# 2. Mapping: density of adhesions by nuts
# 3. Barplots adhesions/type of nuts - fig. 3.14
# 4. Barplots nb adhésion/country
# 5. Barplots nb seats/country


# Working directory huma-num
# setwd("~/BD_Keep_Interreg/ETMUN/")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
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


# Import data

## old
# EtmunPoints <- read.csv2("DataSource/MembersETMUNGeocode.csv", stringsAsFactors = F) 
# ## rm na before transform to sf : removed 75 out of 17333 rows (<1%)
# EtmunPoints <- EtmunPoints %>% filter_at(.vars = c("lon", "lat"), any_vars(!is.na(.)))
# sfAdhesion <- st_as_sf(EtmunPoints, coords = c("lon", "lat"), crs = 4326) %>%
#   st_sf(sf_column_name = "geometry") %>%
#   st_transform(crs = 3035)

## snap points (see snap_etmun.R)
sfETMUN_snap <- readRDS("Data/sfETMUN_snap.RDS")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()

etmun_orga <- readRDS("DataSource/BD_ETMUN_ORGANISATION.rds")

## quelles sont les asso dans lesquelles il y a bcp de villes espagnoles (en absolu)
##esp <- sfETMUN_snap %>% filter(CountryCode == "ES") %>% group_by(Network_Name) %>% summarise(n = n())




# ================ Functions ================ 

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
  
  # list
  ptgrid[["grid"]] <- grid 
  
  # remove null values
  ptgrid[["grid0"]] <- grid %>% 
    filter(n != 0)
  
  return(ptgrid)
}

## Plot a gridded map
plot_grid <- function(grid, adm, frame, sources, titleLeg, labels, labels2){
  
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) # à ajuster
  
  # Plot the map
  choroLayer(grid, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n")
  plot(st_geometry(adm), col = NA, border = "white", lwd = 0.75, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  ## Add legend
  legendChoro(pos = c(1000000, 3000000), 
              title.cex = 0.8, 
              values.cex = 0.7,
              title.txt = titleLeg, 
              breaks = bks, 
              nodata = FALSE, 
              values.rnd = 0, 
              col = cols)
  
  # Add an explanation text
  text(x = 1000000, y = 2700000, labels = labels, cex = 0.7, adj = 0)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = labels2, cex = 0.75)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1.2, 
        adj = 0.935,
        cex = 0.50)
  
}

## plot a points map
plot_points <- function(frame, adm, sf, txtLeg, sources, labels){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0))
  
  sf <- st_intersection(rec, sf)
  
  # Plot the map
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add a legend
  legend(x = 1000000,
         y = 4500000,
         legend = txtLeg, 
         bty = "n",
         cex = 0.8)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = labels, cex = 0.75)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1.2, 
        adj = 0.935,
        cex = 0.50)
  
}

## Plot a choro map
dens_map <- function(frame, bgmap, sf, titleLeg, sources, labels, labels2){
  
  # set the margins
  bb <- st_bbox(frame)
  par(mar = c(0, 0, 0, 0)) 
  
  # Plot
  plot(st_geometry(frame), border = NA, lwd = 0.5, col = NA,
       xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
  plot(st_geometry(bgmap), col = "#f9e8d0", border = "ivory3", lwd = 0.5, add = TRUE)
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
              values.rnd = 0, 
              col = cols)
  
  # Add an explanation text
  text(x = 1000000, y = 2700000, labels = labels, cex = 0.7, adj = 0)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = labels2, cex = 0.75)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1.2, 
        adj = 0.935,
        cex = 0.50)

  
}



  
# ===== 1. Mapping: adhesions/cell 2019 - Fig. 3.13 ===== 


## 50 km cells
europegrided <- pt_in_grid(feat = sfETMUN_snap, adm = sfEU, cellsize = 50000)

## visualize distribution
skim(europegrided[[1]])
hist(europegrided[[1]]$n)

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Nb network
. <- sfETMUN_snap %>% 
  filter(!duplicated(Code_Network))
gridNW <- pt_in_grid(feat = ., adm = sfEU, cellsize = 50000)
sum(gridNW[[1]]$n)

## Plot and save pdf é fig. 3.13
pdf(file = "OUT/europeGrid_etmunall.pdf", width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA / PG, AD, 2019", 
          titleLeg = "Nombre d'adhésions aux associations\nde municipalités par carreau de 2 500 km2*",
          labels = "*Discrétisation en progression\ngéométrique",
          labels2 = str_c(ceiling(sum(europegrided[[1]]$n)/100)*100, " adhésionss\n", 
                          sum(gridNW[[1]]$n), " réseaux"))

dev.off()

## PCT 0 participation : 72% de carreaux vides
summary(europegrided[[1]]$n)
sum(europegrided[[1]]$n)
skim(europegrided[[1]])
nrow(europegrided[[1]])
nrow(europegrided[[1]][europegrided[[1]]$n == 0,])/ nrow(europegrided[[1]]) *100
#mapview(europegrided[[1]])




# ===== 2. Mapping: density of adhesions by nuts ===== 


## Prepare data
### Intersect nuts and participations
. <- st_intersects(nutsUR, sfETMUN_snap)

### Count points in polygons
nutsUR <- st_sf(nutsUR, 
                n = sapply(X = ., FUN = length), 
                geometry = st_geometry(nutsUR))

## Add density to df : nb of participations for 10 000 inhabitants
nutsUR <- nutsUR %>% 
  mutate(density = n / Pop_t_2001 * 100000)

## Display map

### distribution
skim(nutsUR$density)
hist(nutsUR$density)

distrib <- nutsUR %>% filter(density >= 1) 
distrib <- sort(distrib$density)
hist(distrib)
bks <- c(0, getBreaks(v =  distrib, method = "fisher-jenks", nclass = 6))
### defines a set of breaks and colors
# myvar <- nutsUR %>% filter(density > 0) 
# bks <- c(0, getBreaks(v =  myvar$density, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

### Plot and save
pdf(file = "OUT/density_nutsUR_etmunpall.pdf",width = 8.3, height = 5.8)
dens_map(frame = rec, 
         bgmap = sfEU, 
         sf = nutsUR, 
         titleLeg = "Nombre d'adhésions aux associations de municipalités\n par NUTs pour 100 000 habitants*",
         labels = "*Discrétisation selon les seuils naturels\n(fisher-jenks)",
         sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA")
dev.off()



# ===== 3. Barplots adhesions/type of nuts - fig. 3.14 ===== 


## load nuts
nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()

## Prepare data
### Intersect nuts and adhesions
. <- st_intersects(nutsUR, sfETMUN_snap)
### Count points in polygons
nutsUR <- st_sf(nutsUR, 
                n = sapply(X = ., FUN = length), 
                geometry = st_geometry(nutsUR))

# average numbers of adhesions by type of nuts
countUR <- nutsUR %>% 
  group_by(Typo7) %>% 
  summarise(nbm = mean(n)) %>% 
  mutate(Lead = "Adhésions à des associations de municipalités") %>% 
  as.data.frame() %>% 
  select(-geometry)



# create barplots
projNuts <- ggplot(data = countUR, aes(x = reorder(Typo7, -nbm), y = nbm, fill = Lead)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(nbm)), position = position_dodge(0.9), vjust = 1.6, color = "white") +
  labs(x = "Types de NUTS",
       y = "Nombre moyen d'adhésions à des associations de municipalité") +
  scale_fill_manual(values= "#999999") +
  theme_light() +
  labs(caption = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA ; ESPON DB 2013\nPG, AD, 2019") +
  theme(legend.position = "none", legend.title = element_blank(),     
        axis.text.x = element_text(size = 9, angle = 20, vjust = 0.8),
        plot.caption = element_text(size = 6)) 

# display end save
pdf(file = "OUT/adh_nutsUR_etmunall.pdf", width = 8.3, height = 5.8)
projNuts
dev.off()




# ===== 4. Barplots nb adhésion/country ===== 


freq <- as.data.frame(table(sfETMUN_snap$CountryCode))
freq <- freq %>% top_n(n = 20)

top20 <- ggplot(data = freq,
       aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "Identity") +
  geom_text(data = freq %>% top_n(n = 16),
            aes(label = Freq), 
            position = position_dodge(0.9), 
            vjust = 1.4, color = "white", size = 4) +
  labs(x = "", 
       y = "Nombre d'adhésions") +
  theme_light() +
  annotate("text", x = 10, y = 4000, hjust = 0,
           label = paste("Les 20 premiers pays totalisent ", sum(freq$Freq) ," adhésions", sep = ""))


# display end save
pdf(file = "OUT/adh_pays_top20.pdf", width = 8.3, height = 5.8)
top20
dev.off()



# ===== 5. Barplots nb seats/country ===== 

freq <- as.data.frame(table(etmun_orga$'Country (secretariat)'))
freq <- freq %>% top_n(n = 6)

top <- ggplot(data = freq,
                aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_bar(stat = "Identity") +
  geom_text(aes(label = Freq), 
            position = position_dodge(0.9), 
            vjust = 1.4, color = "white", size = 4) +
  labs(x = "", 
       y = "Nombre de sièges d'association") +
  theme_light() +
  annotate("text", x = 2, y = 2, hjust = 0,
           label = "")




