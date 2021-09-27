
##==========================================================================##         
##            CARTOGRAPHIE de la participation aux projets de l'UE          ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base Eucicop/keep / réalisations carto-graphiques          ##
##               du chapitre 3 (carroyage, dot plot, bar plot)              ##
##                                                                          ##
## PG, AD, Octobre 2019                                                     ##
##==========================================================================##


# CONTENTS
# 1. Map participations/cell 2000-2018 - Fig. 3.4 
# 2. Map participations/cell 2000-2006, 2007-2013 et 2014-2020 - Fig. 3.5   
# 3. Barplots numbers of participations by country - Fig. 3.9 
# 4. Map lead partner density  
# 5. Map participations/pop 2006/cell - Fig. 3.10
# 6. Density of participations by nuts 
# 7. Barplots participations/type of nuts - Fig. 3.11 
# 8. ANOVA participations/type of nuts
# 9. Map extreme participation values by type of nuts - fig. 3.12


# Working directory huma-num
setwd("~/BD_Keep_Interreg/KEEP")

setwd("~/git/PG_chap3/Chap3_LocationalAnalysis/KEEP")
options(scipen = 999)

# Library
library(cartography)
library(tidyverse)
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

# Use snap points instead
# participations <- readRDS("Data/Participations_All_Eucicop.RDS")
# partners <- readRDS("Data/UniquePartners_GNid_Eucicop.RDS")
# projects <- readRDS("Data/ProjectsEucicop_all_noduplicated.RDS")

## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")

sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
rec <- st_read("AD/FDCARTE/rec_3035.geojson")

nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>% 
  st_make_valid()



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
  
  # plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA,
  #      xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
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
  
  # # Add a layout
  # layoutLayer(title = "",
  #   sources = sources,
  #   author = "PG, AD, 2019",
  #   horiz = TRUE,
  #   col = NA,
  #   frame = F,
  #   scale = 500,
  #   posscale = c(6500000, 1000000)
  # )
  
}

## Plot 3 maps with a unique legend
plot_grids <- function(grid1, grid2, grid3, title1, title2, title3,
                       adm, frame, sources, titleLeg, labels, summary, 
                       tot1, tot2, tot3){
  
  bb <- st_bbox(frame)
  
  ## plot 3 maps on a single figure 
  par(mar = c(0, 0, 0, 0), mfrow = c(2, 2), ps=15)
  
  ## plot1
  choroLayer(grid1, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n")
  plot(st_geometry(adm), col = NA, border = "white", lwd = 0.2, add = T)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = title1, cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = tot1, cex = 0.5)
  
  ## plot2
  choroLayer(grid2, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n")
  plot(st_geometry(adm), col = NA, border = "white", lwd = 0.2, add = T)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = title2, cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = tot2, cex = 0.5)
  
  ## plot3
  choroLayer(grid3, var = "n", border = NA, breaks= bks, col= cols, 
             legend.pos = "n")
  plot(st_geometry(adm), col = NA, border = "ivory4", lwd = 0.2, add = T)
  plot(st_geometry(frame), border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
  
  # Add title
  text(x = 1000000, y = 5300000, labels = title3, cex = 0.7, adj = 0, font = 2)
  
  # Add total
  text(x = c(bb[3]-1000000), y = c(bb[4]-800000), labels = tot3, cex = 0.5)
  
  
  # Add scalebar
  barscale(500, pos = c(6500000, 1000000))
  
  # # Add sources
  # mtext(text = sources,
  #       side = 4, 
  #       line = -1.3, 
  #       adj = 0.2,
  #       cex =0.35)
  
  # ## plot4
  plot(st_geometry(frame), border = NA, col = NA)
  
  ## Add legend
  legendChoro(pos = c(1000000, 2800000),
              title.cex = 0.7,
              values.cex = 0.6,
              title.txt = titleLeg,
              breaks = bks,
              nodata = FALSE,
              values.rnd = 0,
              cex = 1.3,
              col = cols,
              border = "ivory4")
  
  # Add an explanation text
  text(x = 1000000, y = 2400000, labels = labels, cex = 0.6, adj = 0)
  
  # Add summary
  text(x = 4200000, y = 3800000, labels = "Part de carreaux vides :", cex = 0.6, adj = 0, font = 2)
  text(x = 4200000, y = 3300000, labels = summary, cex = 0.6, adj = 0, font = 1)
  
  # Add sources
  mtext(text = sources,
        side = 1, 
        line = -1, 
        adj = 0.85,
        cex = 0.38)
  
  #plot(myPlot, add = TRUE)

}

## plot a points map
plot_points <- function(frame, adm, sf, txtLeg, sources, labels){
  
  # stock bbox
  bb <- st_bbox(frame)
  
  # Define margins
  par(mar = c(0,0,0,0))
  
  sf <- st_intersection(rec, sf)
  
  # Plot the map
  # plot(st_geometry(frame), border = NA, lwd = 0.5, col = NA,
  #      xlim = bb[c(1,3)], ylim =  bb[c(2,4)])
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
  
  # # Add a layout
  # layoutLayer(title = "",
  #             sources = sources,
  #             author = "PG, AD, 2019",
  #             horiz = FALSE,
  #             col = NA,
  #             frame = F,
  #             scale = 500,
  #             posscale = c(6500000, 1000000)
  # )
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
  
  # # Add a layout
  # layoutLayer(title = "", 
  #             sources = sources, 
  #             author = "PG, AD, 2019", 
  #             horiz = FALSE,
  #             col = NA, 
  #             frame = F, 
  #             scale = 500, 
  #             posscale = c(6500000, 1000000))
  
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



# ==== 1. Map participations/cell 2000-2018 - Fig. 3.4 ==== 


## 2 500 km2 cells
europegrided <- pt_in_grid(feat = sfParticipations_snap, adm = sfEU, cellsize = 50000)

## defines a set of breaks and colors
bks <- c(0, getBreaks(v = europegrided[[2]]$n, method = "geom", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

## Nb projects
. <- sfParticipations_snap %>% 
  filter(!duplicated(ID_PROJECT))
gridProj <- pt_in_grid(feat = ., adm = sfEU, cellsize = 50000)
sum(gridProj[[1]]$n)

## Plot and save pdf - fig. 3.4
pdf(file = "AD/OUT/europeGrid_eucicopall.pdf",width = 8.3, height = 5.8)
plot_grid(grid = europegrided[[1]], 
          adm = sfEU,
          frame = rec,
          sources = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG, AD, 2019", 
          titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2*",
          labels = "*Discrétisation en\nprogression géométrique",
          labels2 = str_c(ceiling(sum(europegrided[[1]]$n)/100)*100, " participations\n", 
                          ceiling(sum(gridProj[[1]]$n)/100)*100, " projets"))
dev.off()

## PCT 0 participation
summary(europegrided[[1]]$n)
sum(europegrided[[1]]$n)
skim(europegrided[[1]])
nrow(europegrided[[1]])
nrow(europegrided[[1]][europegrided[[1]]$n == 0,])/ nrow(europegrided[[1]]) *100




# ==== 2. Map participations/cell 2000-2006, 2007-2013 et 2014-2020 - Fig. 3.5 ==== 


## 2 500 km2 cells
europegrided <- pt_in_grid(feat = sfParticipations_snap, adm = sfEU, cellsize = 50000)

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


## 7280 carreaux
skim(europegrided1[[1]])
skim(europegrided2[[1]])
skim(europegrided3[[1]])
sum(europegrided1[[1]]$n) + sum(europegrided2[[1]]$n) + sum(europegrided3[[1]]$n)

# nrow(europegrided1[[1]][europegrided1[[1]]$n == 0,])/ nrow(europegrided1[[1]]) *100
# nrow(europegrided2[[1]][europegrided2[[1]]$n == 0,])/ nrow(europegrided2[[1]]) *100
# nrow(europegrided3[[1]][europegrided3[[1]]$n == 0,])/ nrow(europegrided3[[1]]) *100

## % empty cells
allGrid <- cbind(europegrided1[[1]], europegrided2[[1]], europegrided3[[1]])
dfSummary <- allGrid %>% 
  as.data.frame() %>% 
  select(-grid, -grid.1, -grid.2) %>%
  summarise("2000-2006" = round(sum(n == 0)/nrow(.)*100),
            "2007-2013" = round(sum(n.1 == 0)/nrow(.)*100),
            "2014-2020" = round(sum(n.2 == 0)/nrow(.)*100))

dfSummary <- dfSummary %>% 
  gather(key = "Period", value = "P0")


# myPlot <- ggplot(data = dfSummary, aes(x = Period, y = P0)) +
#   geom_bar(stat = "identity", width = 0.35) +
#   geom_text(aes(label = paste(P0, " %", sep = "")), position = position_dodge(0.9), vjust = 1.6, color = "white") +
#   labs(x = "",
#        y = "Part de carreaux vides") +
#   theme_light() +
#   labs(caption = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019") +
#   theme(plot.caption = element_text(size = 6))


# ##DO NOT RUN ---
# plot1 <- plot_grid(grid = europegrided[[1]], 
#           adm = sfEU,
#           frame = rec,
#           sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ", 
#           titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2*",
#           labels = "*Discrétisation en\nprogression géométrique")
# gridExtra::grid.arrange(plot1 ,myPlot , nrow = 1)  
# 
# library(cowplot)
# library(gridGraphics)
# cowplot::plot_grid(plot_grids(grid1 = europegrided1[[1]], 
#                               grid2 = europegrided2[[1]],
#                               grid3 = europegrided3[[1]],
#                               title1 = "2000-2006",
#                               title2 = "2007-2013",
#                               title3 = "2014-2020",
#                               adm = sfEU,
#                               frame = rec,
#                               titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2*",
#                               sources = "Source : EUCICOP 2019 / KEEP Closed Projects 2000-2019\nPG, AD, 2019",
#                               labels = "*Discrétisation en\nprogression géométrique"),
#                    myPlot, nrow = 2, ncol = 2)
# 
# cowplot::plot_grid(plot_grid(grid = europegrided[[1]], 
#                              adm = sfEU,
#                              frame = rec,
#                              sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ", 
#                              titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2*",
#                              labels = "*Discrétisation en\nprogression géométrique"),
#                    myPlot, nrow = 1, ncol = 2)
# ### display and save
# pdf(file = "AD/OUT/test.pdf", width = 8.3, height = 5.8)
# gridExtra::grid.arrange(superbeCarte,superbeCarte, superbeCarte, myPlot, nrow = 2, ncol = 2)  
# dev.off()

## Nb projects by period
sfProjet <- sfParticipations_snap %>% 
  group_by(Period) %>% 
  filter(!duplicated(ID_PROJECT))
gridProj1 <- pt_in_grid(feat = sfProjet %>% filter(Period == "2000-2006"), 
                        adm = sfEU, cellsize = 50000)
gridProj2 <- pt_in_grid(feat = sfProjet %>% filter(Period == "2007-2013"), 
                        adm = sfEU, cellsize = 50000)
gridProj3<- pt_in_grid(feat = sfProjet %>% filter(Period == "2014-2020"), 
                       adm = sfEU, cellsize = 50000)
sumProj <- c(sum(gridProj1[[1]]$n), sum(gridProj2[[1]]$n), sum(gridProj3[[1]]$n))



## display maps and save pdf - fig. 3.5
pdf(file = "AD/OUT/europeGridPeriod_eucicopall.pdf", width = 8.3, height = 5.8)
plot_grids(grid1 = europegrided1[[1]], 
           grid2 = europegrided2[[1]],
           grid3 = europegrided3[[1]],
           title1 = "2000-2006",
           title2 = "2007-2013",
           title3 = "2014-2020",
           adm = sfEU,
           frame = rec,
           titleLeg = "Nombre de participations\naux projets de l'UE\npar carreau de 2 500 km2*",
           sources = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG, AD, 2019",
           labels = "*Discrétisation en\nprogression géométrique",
           summary = "2000-2006 = 75%\n2007-2013 = 72%\n2014-2020 = 84%",
           tot1 = str_c(floor(sum(europegrided1[[1]]$n)/100)*100, " participations\n", 
                        sumProj[1], " projets"),
           tot2 = str_c(ceiling(sum(europegrided2[[1]]$n)/100)*100, " participations\n", 
                        ceiling(sumProj[2]/100)*100, " projets"),
           tot3 = str_c(floor(sum(europegrided3[[1]]$n)/100)*100, " participations\n", 
                        floor(sumProj[3]/100)*100, " projets"))
dev.off()

rm(list = ls()) 


# ==== 3. Barplots numbers of participations by country - Fig. 3.9 ==== 


## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")

## Add ISO to europe shape
sfEU <- st_read("AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)
CORRESP_CNTR_ISO2 <- read_delim("AD/CORRESP_CNTR_ISO2.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
sfEU <- left_join(select(sfEU, ID, NAME_EN, UE28), 
                   select(CORRESP_CNTR_ISO2, NAME_EN = COUNTRY, ISO_SF = ISO_A2), 
                   by = "NAME_EN")


## Count participations/country/period
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

## nb projects
nbproj <- sfParticipations_snap %>% 
  st_intersection(., select(sfEU, NAME_EN, ISO_SF, UE28)) %>% 
  filter(UE28 == TRUE)
length(unique(nbproj$ID_PROJECT))
  

## Plot participation/country ~ fig. 3.9
partCntr <- ggplot(data = dfpartCntrUE, 
                   aes(x = reorder(NAME_EN, -nbp_c), y = nbp_c)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = nbp_c), 
            position = position_dodge(0.9), 
            vjust = 1.6, color = "white", size = 2) +
  labs(x = "UE28",
       y = "Nombre de participations en Europe sur la période 2000-2019") +
  annotate(geom = "label", x = 20, y = 6000, hjust = 0, size = 3,
           label= paste0(sum(dfpartCntr$nbp_cp), " participations\n",
                         length(unique(nbproj$ID_PROJECT)), " projets")) +
  theme_light() +
  labs(caption = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019\nPG, AD, 2019") +
  theme(legend.position = c(0.6, 0.8),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.caption = element_text(size = 6)) 

## display end save
pdf(file = "AD/OUT/particip_country_eucicopall.pdf", width = 8.3, height = 5.8)
partCntr
dev.off()


## faceting participation/country/period ~ not used in the text
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




# ==== 4. Map lead partner density & dots plot ==== 


## data with snaped points 
sfParticipations_snap <- readRDS("Data/sfParticipations_snap.RDS")


## count 
. <- st_intersection(rec, sfParticipations_snap) %>% 
  filter(Lead.Partner == "Yes")
nb <- c(length(unique(.$ID_PARTICIPATION)), 
        length(unique(.$ID_PARTNER)), 
        length(unique(.$ID_PROJECT)))

## Dots map
### Display points and save
pdf(file = "AD/OUT/densityLead_eucicopall.pdf", width = 8.3, height = 5.8)
plot_points(frame = rec,
            adm = sfEU,
            sf = sfParticipations_snap %>% filter(Lead.Partner == "Yes"),
            txtLeg = "Chaque point représente\nune participation d'un lead partner\naux projets de l'UE",
            source = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG, AD, 2019",
            labels = str_c(ceiling(nb[2]/100)*100, " lead partners\n", 
                           floor(nb[3]/100)*100, " projets"))
dev.off()


## Display density cells for lead partners only
### 2 500 km2 cells
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
          sources = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 / PG, AD, 2019", 
          titleLeg = "Nombre de participations des lead partners\naux projets de l'UE\npar carreau de 2 500 km2*",
          labels = "*Discrétisation en\nprogression géométrique",
          labels2 = "")
dev.off()




# ==== 5. Map participations/pop 2006/cell - Fig. 3.10 ==== 


## see script 01b_interpolation.R for building pop interpolate grid

## load interpolate grid pop
interpolate_grid <- readRDS("AD/SHP/interpolation.RDS")
interpolate_grid <- interpolate_grid %>% select(-Group.1)

### Intersect interpolate_grid and participations
. <- st_intersects(interpolate_grid, sfParticipations_snap)
### Count points in polygons
grid <- st_sf(n = sapply(X = ., FUN = length),
             geometry = st_geometry(interpolate_grid))
sum(grid$n)

### add pop
grid <- st_join(interpolate_grid, grid, join = st_equals)
mapview(grid)


## Add density to df : nb of participations for n inhabitants
vec <- 10000
grid <- grid %>% 
  mutate(density = n / POP_TOT * vec)


## Display map

### distribution
# skim(grid$density)
# hist(grid$density)

### supression des valeurs extrêmes (density < 100) 
distrib <- grid %>% filter(density >= 1 & density < 100) 
distrib <- sort(distrib$density)


### defines a set of breaks and colors
bks <- c(0, getBreaks(v =  distrib, method = "geom", nclass = 6))
#bks <- c(0, getBreaks(v =  distrib, method = "fisher-jenks", nclass = 6), 145000)
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 1))

### Plot and save
pdf(file = "AD/OUT/density_popgrid_eucicopall.pdf",width = 8.3, height = 5.8)
dens_map(frame = rec, 
         bgmap = sfEU, 
         sf = grid %>% filter(density < 100), 
         titleLeg = "Nombre de participations\naux projets de l'UE\npour 10 000 habitants\net par carreau de 2 500km2*",
         sources = "Sources : EUCICOP 2019 ; KEEP Closed Projects 2000-2019 ; ESPON DB 2013 ; Geostat 2006 / PG, AD 2020",
         labels = "*Discrétisation en\nprogression géométrique",
         labels2 = str_c(floor(sum(grid$n)/100)*100, " participations\n19000 projets"))
dev.off()




# ==== 6. Density of participations by nuts ==== 


## Prepare data
### Intersect nuts and participations
inter <- st_intersects(nutsUR, sfParticipations_snap)
inter2 <- st_intersects(nutsUR, sfParticipations_snap %>% filter(Lead.Partner == "Yes"))
### Count points in polygons
nutsUR <- st_sf(nutsUR, 
                n = sapply(X = inter, FUN = length), 
                nL = sapply(X = inter2, FUN = length),
                geometry = st_geometry(nutsUR))

## Add density to df : nb of participations for 100 000 inhabitants
nutsUR <- nutsUR %>% 
  mutate(density = n / Pop_t_2001 * 10000,
         density2 = n /Pop_t_2001 * 100000)

## Display map

### distribution
skim(nutsUR$density)
hist(nutsUR$density)

distrib <- nutsUR %>% filter(density >= 1) 
distrib <- sort(distrib$density)
hist(distrib)

### defines a set of breaks and colors
# myvar <- nutsUR %>% filter(density != 0) %>% select(density) 
# bks <- c(0, getBreaks(v =  myvar$density, method = "geom", nclass = 6))
bks <- c(0, getBreaks(v =  distrib, method = "fisher-jenks", nclass = 6))
cols <- c("#e5dddb", carto.pal("turquoise.pal", length(bks) - 2))

### Plot and save
pdf(file = "AD/OUT/density_nutsUR_eucicopall.pdf",width = 8.3, height = 5.8)
dens_map(frame = rec, 
        bgmap = sfEU, 
        sf = nutsUR, 
        titleLeg = "Nombre de participations\naux projets de l'UE\npour 10 000 habitants et par NUTs*",
        sources = "Sources : EUCICOP 2019 / KEEP Closed Projects 2000-2019 ; ESPON DB 2013",
        labels = "*Discrétisation en\nprogression géométrique")
dev.off()




# ==== 7. Barplots participations/type of nuts - Fig. 3.11 ==== 

# ## Prepare data
# ### Intersect nuts and participations
# inter <- st_intersects(nutsUR, sfParticipations_snap)
# inter2 <- st_intersects(nutsUR, sfParticipations_snap %>% filter(Lead.Partner == "Yes"))
# ### Count points in polygons
# nutsUR <- st_sf(nutsUR, 
#                 n = sapply(X = inter, FUN = length), 
#                 nL = sapply(X = inter2, FUN = length),
#                 geometry = st_geometry(nutsUR))
# rm(inter, inter2)

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
                   Lead = "Lead partners uniquement")
bibi <- rbind(bibi, bibi2)
rm(bibi2)
# create barplots
projNuts <- ggplot(data = bibi, aes(x = reorder(Typo7, -nbm), y = nbm, fill = Lead)) +
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

# display and save
pdf(file = "AD/OUT/particip_nutsUR_eucicopall.pdf", width = 8.3, height = 5.8)
projNuts
dev.off()




# ==== 8. ANOVA participations/type of nuts ==== 

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
sf_nutsUR <- countP(sf_nutsUR, sfParticipations_snap)
nutsUR <- sf_nutsUR %>% as.data.frame() %>% select(-geometry) 
sum(nutsUR$n)

## Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = nutsUR, varx = c("Typo7_v2"), vary = c("n")) 
resume <- ComputeRegression(nutsUR, vardep = "n", varindep = "Typo7_v2")
resume$TABCOEF
### R2 = 18%

### save tab
require(gridExtra)
require(grid)
pdf(file = "AD/OUT/ANOVAtab_particip_nutsUR_keep.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
grid.table(anovaTab, rows = NULL)
dev.off()

## Anova plot
anovaPlot <- AnovaPlot(df = nutsUR, varx = c("Typo7_v2"), vary = c("n"), 
                       tx = "Type de Nuts",
                       ty = "Nombre de participations") 

### save plot
pdf(file = "AD/OUT/ANOVAboxplot_particip_nutsUR_keep.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
anovaPlot
dev.off()



# ==== 9. Map extreme participation values by type of nuts - fig. 3.12 ==== 

## Prepare data
### load nuts
sf_nutsUR <- st_read("../OtherGeometry/NUTS_UrbainRural.geojson", crs = 3035) %>%
  st_make_valid()

### first recode variable Typo7
sf_nutsUR <- sf_nutsUR %>% 
  mutate(Typo7_v2 = recode(Typo_7Clv2,
                           "4" = "Régions sous dominance\nd'une métropole",         
                           "6" = "Régions avec densité\nurbaine élevée",            
                           "5" = "Régions à majorité\nde villes moyennes",         
                           "7" = "Régions avec densité\nurbaine et rurale élevées",   
                           "1" = "Régions rurales\nsous influence métropolitaine",
                           "2" = "Régions rurales\navec villes petites et moyennes",
                           "3" = "Régions rurales isolées"))

### Count participations in each nuts
sf_nutsUR <- countP(sf_nutsUR, sfParticipations_snap)

### Keep extreme values
extr_nuts <- sf_nutsUR %>% 
  filter(n > (mean(n) + 2*sd(n))) %>% #798.9331
  select(Nuts_Id, Name, Typo7_v2, n)

### typo as factor
unique(extr_nuts$Typo7_v2)
extr_nuts$TYPO <- factor(extr_nuts$Typo7_v2,
                         levels = c("Régions sous dominance\nd'une métropole",
                                    "Régions avec densité\nurbaine élevée",
                                    "Régions à majorité\nde villes moyennes",
                                    "Régions avec densité\nurbaine et rurale élevées",
                                    "Régions rurales\navec villes petites et moyennes",
                                    "Régions rurales isolées"))

### rename nuts
extr_nuts <- extr_nuts %>% 
  mutate(name2 = recode(Name, "Prov. West-Vlaanderen" = "1",
                              "Bruxelles-Capitale" = "Bruxelles",
                              "Jihocecký kraj" = "10",
                              "Jihomoravský kraj" = "9",
                              "Moravskoslezský kraj" = "Moravskoslezský",
                              "Nord (FR)" = "2",
                              "Northern Ireland (UK)" = "Northern Ireland",
                              "Mecklenburg-Vorpommern" = "13",
                              "Dusseldorf//Arnsberg" = "4",
                              "Sjeverozapadna Hrvatska" = "6",
                              "Milano//Varese//Como" = "12",
                              "Stockholms län" = "Stockholms",
                              "Skåne län" = "Skåne",
                              "Västerbottens län" = "Västerbottens",
                              "Gelderland" = "3",
                              "Osrednjeslovenska" = "5",
                              "Steiermark" = "7",
                              "Niederösterreich" = "8",
                              "Île de France" = "IdF",
                              "Oberbayern" = "11"))


indexlab <- c("1: West-Vlaanderen
2: Nord
3: Gelderland
4: Dusseldorf/Arnsberg
5: Osrednjeslovenska
6: Sjeverozapadna Hrvatska
7: Steiermark
8: Niederösterreich
9: Jihomoravský
10: Jihocecký
11: Oberbayern
12: Milano/Varese/Como
13: Mecklenburg-Vorpommern")
                                       
## mapping with ggplot

mapview(extr_nuts)


### Use same colors as Barplot participations by typo NUTS U/R (cf. 04_explo_urbact.R)
colNuts <- c("#135D89", "#4D95BA", "#96D1EA", 
           "#9F7C59", "#36842E", "#7CB271")

### stock bbox of extreme nuts
bb <- st_bbox(extr_nuts)



# devtools::install_github("yutannihilation/ggsflabel")
# require(ggsflabel)

topoup <- c("Northern Ireland", "Sevilla", "Barcelona", "Roma", "Attiki", "Thessaloniki",
            "Csongrád", "Venezia", "Torino", "Bruxelles", "Dresden", "Skåne",
            "Stockholms", "Põhja-Eesti", "Oberbayern", "Île de France", "Bas-Rhin",
            "Schleswig-Holstein", "Västerbottens")
toponum <- c("1", "2", "3", "5", "6", "7", "8", "9", "10", "12")
toporepel <- c("Wien", "Budapest", "Niederösterreich", "Jihomoravský", "Münster",
               "Freiburg", "Mecklenburg-\nVorpommern")

bibi <- st_as_sfc(bb) %>% st_sf
plot(bibi)

### create a simple and pretty scale bar 500km
myScaleBar <- data.frame(X = c(c(bb[3]-900000), c(bb[3]-400000)),
                         Y = c(c(bb[2]+200000), c(bb[2]+200000)))
myScaleBar <- data.frame(X = c(c(bb[3]+500000), c(bb[3]+1000000)),
                         Y = c(c(bb[2]+200000), c(bb[2]+200000)))

pdf(file = "AD/OUT/extrPart_nutsUR_keep.pdf", width = 8.3, height = 5.8)
ggplot() + 
  geom_sf(data = sfEU, fill = "grey80", color = "grey60", lwd = 0.5) +
  geom_sf(data = extr_nuts, aes(fill = TYPO), colour = "grey60", lwd = 0.4, show.legend = TRUE) +
  scale_fill_manual(name = "Typologie urbain/rural\n des NUTs (2 & 3)*", values = colNuts) +
  ggsflabel::geom_sf_text_repel(data = extr_nuts %>% filter(!name2 %in% toponum),
                                aes(label = name2), size = 2.5, color = "#4d4d4d",
                                force = 1, nudge_x = 20, direction = "x",
                                segment.size = 0.1, min.segment.length = 0.2) +
  geom_sf_text(data = extr_nuts %>% filter(name2 %in% toponum),
               aes(label = name2), size = 2.5, color = "#4d4d4d",
               check_overlap = TRUE) +
  annotate("text", label = "*Seuls apparaissent sur la carte\nles NUTs comptant au moins\n368 participations\n(n>mean(n) + 2*sd(n))",
           size = 2.8, hjust = 0,
           x = c(bb[3]+120000), y = c(bb[2]+1800000)) +
  annotate("text", label = indexlab,
           size = 2.2,  hjust = 0, vjust = 1,
           x = c(bb[1]-200000), y = c(bb[4])) +
  annotate("text", label = "EUCICOP 2019 ; ESPON DB, 2013 / PG, AD, 2020",
           size = 2.2, 
           hjust = 1, 
           x = c(bb[3]+1250000), y = c(bb[2]-70000)) +
  geom_line(data = myScaleBar, aes(x = X, y = Y), size = 0.5, color = "#333333") +
  annotate("text", label = "500 km", size = 2.5, color = "#333333", hjust = 0,
           x = c(bb[3]+680000), y = c(bb[2]+250000)) +
  #geom_sf(data = bibi, fill = NA, color = "ivory4", size = 0.5) +
  coord_sf(xlim = c(bb[1]-250000, bb[3]+1300000), 
           ylim =  c(bb[2]-100000, bb[4]+100000), 
           datum = sf::st_crs(3035), expand = FALSE) +
  theme_void() +
  theme(legend.position = c(0.85, 0.8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7.5),
        rect = element_rect(fill = NA, colour = "ivory4",
                            size = 0.5, linetype = 1),
        legend.background = element_rect(colour = NA),
        legend.key = element_rect(fill = NA, colour = NA))
dev.off()



### version de travail
pdf(file = "AD/OUT/extrPart_nutsUR_keep.pdf", width = 8.3, height = 5.8)
par(mar = c(0, 0, 0, 0)) 
plot(st_geometry(sfEU), col = "ivory3", border = "ivory4", lwd = 0.5,
     xlim = bbox[c(1,3)], ylim =  bbox[c(2,4)])
typoLayer(extr_nuts, var = "Typo7_v2", 
          legend.values.order = breaks,
          col = colNuts,
          lwd = 0.5, legend.pos = "topleft", 
          legend.title.txt = "Typologie urbaun/rural\ndes NUTS (2 & 3)", add = TRUE)
#plot(bibi, border = "ivory4", lwd = 0.5, col = NA, add = TRUE)
labelLayer(extr_nuts, txt = "Name",  cex = 0.6,   
           font = 3, overlap = FALSE, show.lines = TRUE)
barscale(500, pos = c(bbox[3], bbox[2]))
mtext(text = "EUCICOP 2019 ; ESPON DB, 2013 / PG, AD, 2020",
      side = 1, 
      line = -1.2, 
      adj = 0.935,
      cex = 0.50)
dev.off()



### ==== 9.bIs Exploration top 3 of extrem nuts to comment Map ==== 

NUTsExtremID <- extr_nuts$Nuts_Id %>% as.character()

ParticNuts <- st_join(sfParticipations_snap, nutsUR, join= st_intersects)

ParticNutsExtrem <- ParticNuts %>% mutate(Nuts_Id = as.character(Nuts_Id))%>% 
  filter(Nuts_Id %in% NUTsExtremID)

ProgTop3Nuts <- ParticNutsExtrem %>%mutate(Name= as.character(Name))%>% st_drop_geometry()%>%
                group_by(Name, Programme)%>% tally() %>% top_n(3)


library(ggforce)

for(i in 1:round(length(unique(ProgTop3Nuts$Name))/4)){
  
print(ggplot(ProgTop3Nuts)+
   geom_bar(aes(x = Programme, y = n),  stat="identity")+ 
  facet_wrap_paginate(~Name, scales = "free", nrow = 4, ncol = 1, strip.position = "top", as.table = F, page = i) + 
    coord_flip())

  }



mapview(sf_nutsUR)
