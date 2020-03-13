
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
# 4. ANOVA adhesions/type of nuts
# 5. Barplots nb seats/country
# 6. Barplots nb seats/country
# 7. Evolution maps of EFUS & ICLEI networks - fig. 3.?


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

## dot plot - 4 maps !! Check NA !!
plot_points_grid <- function(frame, adm, sf, sources){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0), mfrow = c(2, 2), ps=15)
  
  # Prepare data
  sk <- skim(sf$YEAR)
  
  sf <- st_intersection(rec, sf)
  sf1 <- sf %>% filter(YEAR <= sk$formatted[7])
  sf2 <- sf %>% filter(YEAR > sk$formatted[7] & YEAR <= sk$formatted[8])
  sf3 <- sf %>% filter(YEAR > sk$formatted[8] & YEAR <= sk$formatted[9])
  sf4 <- sf %>% filter(YEAR > sk$formatted[9] & YEAR <= sk$formatted[10])
  
  sf50 <- sf %>% filter(YEAR <= sk$formatted[8]) 
  sf75 <- sf %>% filter(YEAR <= sk$formatted[9])
  st <- c(nrow(sf1), nrow(sf50), nrow(sf75), nrow(sf))
  
  # Plot the map 1
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf1), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = str_c("En", sk$formatted[7], sep = " "),
       cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), 
       labels = str_c(sk$stat[7], " :\n", st[1], " adhésions", sep = ""), cex = 0.75)
  
  # Plot the map 2
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf1), col = "grey30", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(sf2), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = str_c("En", sk$formatted[8], sep = " "),
       cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), 
       labels = str_c(sk$stat[8], " :\n", st[2], " adhésions", sep = ""), cex = 0.75)
  
  # Plot the map 3
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf1), col = "grey30", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(sf2), col = "grey30", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(sf3), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = str_c("En", sk$formatted[9], sep = " "),
       cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), 
       labels = str_c(sk$stat[9], " :\n", st[3], " adhésions", sep = ""), cex = 0.75)
  
  # Plot the map 4
  plot(st_geometry(adm), col = "ivory4")
  plot(st_geometry(sf), col = "grey30", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(sf4), col = "#ff6208", pch = 20, cex = 0.5, add = TRUE)
  plot(st_geometry(adm), col = NA, border = "ivory3", lwd =0.3, add = TRUE)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = str_c("En", sk$formatted[10], sep = " "),
       cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), 
       labels = str_c(sk$stat[10], " :\n", st[4], " adhésions", sep = ""), cex = 0.75)
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1, 
        adj = 0.85,
        cex = 0.38)
  
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

## ANOVA
## Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
## APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
## CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

### Function Anova parameters (1 factor)  
AnovaTab <- function(df, varx, vary){
  groupMean <- round(tapply(df[, vary], df[, varx], mean, na.rm = TRUE), digits = 2)
  groupMedian <- round(tapply(df[, vary], df[, varx], median, na.rm = TRUE), digits = 2)
  groupVar <- round(tapply(df[, vary], df[, varx], var, na.rm = TRUE), digits = 2)
  tabGroup <- data.frame(Modalité = names(groupMean), 
                         Moyenne = groupMean,
                         Médiane = groupMedian,
                         Variance = groupVar, 
                         stringsAsFactors = FALSE)
  tabAll <- data.frame(Modalité = "Ensemble", 
                       Moyenne = round(mean(df[, vary]), digits = 2), 
                       Médiane = round(median(df[, vary]), digits = 2), 
                       Variance = round(var(df[, vary]), digits = 2), 
                       stringsAsFactors = FALSE)
  
  tabVariance <- rbind(tabGroup, tabAll)
  
  return(tabVariance)
}

### Anova plot (1 factor) -- modifié
AnovaPlot <- function(df, varx, vary, tx, ty){
  
  xLevels <- sort(unique(df[, varx]))
  df$ID <- df[, varx]
  df$VAR <- df[, vary]
  
  if(length(xLevels) == 2){
    colPal <- brewer.pal(n = 3, name = "Set1")[1:2]
  } else if (length(xLevels) > 2){
    colPal <- brewer.pal(n = length(xLevels), name = "Set1")
  }
  
  # jitter points
  set.seed(99)
  df$JIT <- as.numeric(as.factor(df[, varx])) + sample(x = seq(-0.3, 0.3, 0.01), size = nrow(df), replace = TRUE)
  
  # mean segments
  groupMean <- tapply(df[, vary], df[, varx], mean, na.rm = TRUE)
  avgSegment <- data_frame(ID = names(groupMean), 
                           XMIN = seq(1, length(groupMean), 1) - 0.4,  
                           XMAX = seq(1, length(groupMean), 1) + 0.4, 
                           YMIN = groupMean, 
                           YMAX = groupMean)
  
  # residuals segments
  df <- df %>% left_join(x = ., y = avgSegment[, c(1, 4)], by = "ID")
  
  aovPlot <- ggplot() +
    geom_hline(yintercept = mean(df$VAR, na.rm = TRUE), color = "grey60", size = 1, linetype = 2) +
    geom_segment(data = avgSegment, aes(x = XMIN, xend = XMAX, y = YMIN, yend = YMAX), color = "grey40", size = 2) +
    geom_segment(data = df, aes(x = JIT, xend = JIT, y = YMIN, yend = VAR), color = "grey40", alpha = 0.5) +
    geom_point(data = df, aes(JIT, VAR, color = ID), show.legend = FALSE) +
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = tx, breaks = seq(1, length(groupMean), 1), labels = xLevels) +
    scale_y_continuous(name = ty) +
    labs(x = tx, y = ty) +
    theme_bw() +
    labs(caption = "Source : ETMUN, 2019 / PG, AD, 2019") +
    theme(axis.text.x = element_text(size = 9, angle = 20, vjust = 0.6),
          plot.caption = element_text(size = 6))
  
  return(aovPlot)
}

### Compute linear model -- 
ComputeRegression <- function(df, vardep, varindep, interact = FALSE){
  if(interact == FALSE){
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "+")))), data = df)
    linModSumry <- summary(linMod)
  } else {
    linMod <- lm(formula = formula(eval(paste(vardep, "~", paste(varindep, collapse = "*")))), data = df)
    linModSumry <- summary(linMod)
  }
  coefReg <- round(linModSumry$coefficients, digits = 4)[, 1:2]
  rawR2 <- round(linModSumry$r.squared, digits = 2)
  adjR2 <- round(linModSumry$adj.r.squared, digits = 2)
  
  tabResid <- data.frame(ABSRESID = round(linModSumry$residuals, digits = 3), 
                         RELRESID = round(linModSumry$residuals / (df[, vardep] - linModSumry$residuals), digits = 3))
  
  tabResults <- data.frame(CONCEPT = c("Coef. de détermination",
                                       "Coef. de détermination ajusté",
                                       row.names(coefReg)),
                           VALEUR = c(rawR2, adjR2, coefReg[, 1]),
                           stringsAsFactors = FALSE)
  
  return(list(TABCOEF = tabResults, TABRESID = tabResid, COEF = coefReg))
}

## Count participations in Nuts
countP <- function(df1, df2){
  
  ### Intersect umz and participations
  inter <- st_intersects(df1, df2)
  ### Count points in polygons
  df1 <- st_sf(df1, 
               n = sapply(X = inter, FUN = length), 
               geometry = st_geometry(df1))
  return(df1)
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



# ===== 4. ANOVA adhesions/type of nuts ===== 


## load nuts
sf_nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()

## first recode variable Typo7
sf_nutsUR <- sf_nutsUR %>% 
  mutate(Typo7_v2 = recode(Typo_7Clv2,
                           "4" = "Régions sous dominance\nd'une métropole",         
                           "6" = "Régions avec densité\nurbaine élevée",            
                           "5" = "Régions à majorité\nde villes moyennes",         
                           "7" = "Régions avec densité\nurbaine et rurale élevées",   
                           "1" = "Régions rurales\nsous influence métropolitaine",
                           "2" = "Régions rurales\navec villes petites et moyennes",
                           "3" = "Régions rurales isolées"))


## Count participations in each nuts
sf_nutsUR <- countP(sf_nutsUR, sfETMUN_snap)
nutsUR <- sf_nutsUR %>% as.data.frame() %>% select(-geometry) 
sum(nutsUR$n)

## Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = nutsUR, varx = c("Typo7_v2"), vary = c("n")) 
resume <- ComputeRegression(nutsUR, vardep = "n", varindep = "Typo7_v2")
resume$TABCOEF
### R2 = 11%

### save tab
require(gridExtra)
require(grid)
pdf(file = "OUT/ANOVAtab_adh_nutsUR_etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
grid.table(anovaTab, rows = NULL)
dev.off()

## Anova plot
anovaPlot <- AnovaPlot(df = nutsUR, varx = c("Typo7_v2"), vary = c("n"), 
                       tx = "Type de Nuts",
                       ty = "Nombre dadhésions") 

### save plot
pdf(file = "OUT/ANOVAboxplot_adh_nutsUR_etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
anovaPlot
dev.off()


# ===== 5. Barplots nb adhésion/country ===== 


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



# ===== 6. Barplots nb seats/country ===== 

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




# ===== 7. Evolution maps of EFUS & ICLEI networks - fig. 3.? ===== 


##EFUS
efus <- sfETMUN_snap %>% 
  filter(Network_Name == "EFUS") %>% 
  mutate(YEAR = as.numeric(Year_of_Joining))

mapview(efus)

sort(unique(efus$YEAR))
table(efus$YEAR)
skim(efus$YEAR)


##ECLEI
iclei <- sfETMUN_snap %>% 
  filter(Network_Name == "ICLEI")%>% 
  mutate(YEAR = as.numeric(Year_of_Joining))

mapview(iclei)

sort(unique(iclei$YEAR))
table(iclei$YEAR)
skim(iclei$YEAR)


## save pdf EFUS
pdf(file = "OUT/efus_evol.pdf", width = 8.3, height = 5.8)
plot_points_grid(frame = rec, adm = sfEU, sf = efus, 
            sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA / PG, AD, 2020") 
dev.off()


## save pdf ICLEI
pdf(file = "OUT/iclei_evol.pdf", width = 8.3, height = 5.8)
plot_points_grid(frame = rec, adm = sfEU, sf = iclei, 
                 sources = "Sources : ETMUN, Gourdon, 2019 ; Yearbook of International Organizations 2015, UIA / PG, AD, 2020") 
dev.off()
