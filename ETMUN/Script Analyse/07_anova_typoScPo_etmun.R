
##==========================================================================##         
##            ANALYSE BIVARIEE : MEMBRES ETMUN  et TYPO SCIENCESPO          ##
##                                                                          ##
##                                                                          ##    
## DESCRIPTION : Base ETMUN /                                               ##
##                                                                          ##
## PG, AD, mars 2020                                                        ##
##==========================================================================##



# CONTENTS

# Working directory huma-num
# setwd("~/BD_Keep_Interreg/ETMUN")

setwd("~/git/Chap3_LocationalAnalysis/ETMUN")
options(scipen = 999)

# Library
library(tidylog)
library(skimr)
library(tidyverse)
library(sf)
library(mapview)
library(cartography)
library(stringr)
library(readr)


# Import data

## snap points (see snap_etmun.R)
sfETMUN_snap <- readRDS("Data/sfETMUN_snap.RDS")

sfEU <- st_read("../KEEP/AD/FDCARTE/fondEuropeLarge.geojson", crs = 3035)

rec <- st_read("../KEEP/AD/FDCARTE/rec_3035.geojson")

typo <- read_delim("../CountryInfo_PoliticalTypo.csv", 
              ";", escape_double = FALSE, trim_ws = TRUE)

# Prepare data

# count n adhesion by country
country <- sfETMUN_snap %>% 
  group_by(CountryCode, CountryName) %>% 
  summarise(n = n()) %>% 
  st_drop_geometry()

# join typo to country sum
typo <- typo %>% rename(CountryCode = iso_a2)
country <- country %>% 
  left_join(., typo, by = "CountryCode") 


unique(country$LocGovType_PoliticalLeadership)


# ---- ANOVA functions ----
# Commenges H. (2016) ExploratR : outil interactif d'exploration statistique uni- bi- tri- et multi-variée avec R, UMR 8504 Géographie-cités. 
# APPLI : https://analytics.huma-num.fr/geographie-cites/ExploratR/
# CODE : https://zenodo.org/record/155333#.XdZn7dVCfIU

## Anova parameters (1 factor) --
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

## Anova plot (1 factor) -- labels not generalized
AnovaPlot <- function(df, varx, vary, tx, ty, source){
  
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
    geom_label_repel(data = df, 
                     aes(JIT, VAR, label = CountryName), # not generalized
                     na.rm = TRUE, nudge_y = 0.05, color = "black", size = 2.5) +
    scale_color_manual(values = colPal) +
    scale_x_continuous(name = tx, breaks = seq(1, length(groupMean), 1), labels = xLevels) +
    scale_y_continuous(name = ty) +
    labs(x = tx, y = ty) +
    theme_bw() +
    labs(caption = source) +
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



## ----~Apply function: HorizontalPwrRelation ----

unique(country$LocGovType_HorizontalPwrRelation)

## df
HP <- country %>% 
  filter(!is.na(LocGovType_HorizontalPwrRelation)) %>% 
  filter(LocGovType_HorizontalPwrRelation != "OutsideTypology") %>% 
  as.data.frame()

## Stat summary 
resume <- ComputeRegression(HP, vardep = "n", varindep = "LocGovType_HorizontalPwrRelation")
resume$TABCOEF
## R2 = 15% (43 obs)
## R2 = 10% wo outsideTypo (28 obs)


## ----~Apply function: VerticalPwrRelation ----

unique(country$LocGovType_VerticalPwrRelation)

## df
VP <- country %>% 
  filter(!is.na(LocGovType_VerticalPwrRelation)) %>% 
  filter(LocGovType_VerticalPwrRelation != "OutsideTypology") %>% 
  as.data.frame()

### Stat summary 
resume <- ComputeRegression(VP, vardep = "n", varindep = "LocGovType_VerticalPwrRelation")
resume$TABCOEF
## R2 = 38% (43 obs)
## R2 = 28% wo outsideTypo (18%)


## ----~Apply function: PoliticalLeadership ----

unique(country$LocGovType_PoliticalLeadership)

## df
PL <- country %>% 
  filter(!is.na(LocGovType_PoliticalLeadership)) %>% 
  filter(LocGovType_PoliticalLeadership != "OutsideTypology") %>% 
  as.data.frame()

### Stat summary (varx = quali, vary = quanti)
anovaTab <- AnovaTab(df = PL, varx = c("LocGovType_PoliticalLeadership"), 
                     vary = c("n"), int) 
resume <- ComputeRegression(PL, vardep = "n", varindep = "LocGovType_PoliticalLeadership")
resume$TABCOEF
## R2 = 43% (43 obs)
## R2 = 32% wo outsideTypo (17 obs)

### save tab
require(gridExtra)
require(grid)
pdf(file = "OUT/ANOVAtab_adh_typoScPo_etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
grid.table(anovaTab, rows = NULL)
dev.off()


### Anova plot
require(RColorBrewer)
require(ggrepel)
anovaPlot <- AnovaPlot(df = PL, c("LocGovType_PoliticalLeadership"), c("n"),
                       tx = "",
                       ty = "Nombre d'adhésions au réseau ETMUN",
                       source = "Source : ETMUN, 2019 / PG, AD, 2019")

### save plot
pdf(file = "OUT/ANOVAboxplot_adh_typoScPo__etmun.pdf", width = 8.3, height = 5.8, pagecentre = FALSE)
anovaPlot
dev.off()


## ----~Apply function: MeanLAI_9014 ----

skim(country$MeanLAI_9014)

## df
ML <- country %>% 
  filter(!is.na(MeanLAI_9014)) %>% 
  filter(MeanLAI_9014 != "OutsideTypology") %>% 
  as.data.frame()

ML$MeanLAI_9014 <- str_replace(ML$MeanLAI_9014, ",", ".")
ML$MeanLAI_9014 <- as.numeric(as.character(ML$MeanLAI_9014))

### Stat summary (varx = quali, vary = quanti)
resume <- ComputeRegression(PL, vardep = "n", varindep = "MeanLAI_9014")
resume$TABCOEF

reg <- lm(n ~ MeanLAI_9014 , data = ML)
summary(reg)$r.squared

regML <- ggplot(ML, aes(x = MeanLAI_9014, y = n)) +
  geom_point () +
  theme_light() +
  labs(x = "MeanLAI_9014", 
       y = "Nombre d'adhésions à des associations de municipalités") +
  geom_abline(intercept = reg$coefficients[1], slope = reg$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 15, y = 4000, label = paste0("R2 = ", 
                                                              round(summary(reg)$r.squared, 2))) 
regML


ML2 <- ML %>% 
  filter(!CountryCode %in% c("IT", "ES"))
reg2 <- lm(n ~ MeanLAI_9014 , data = ML2)
summary(reg2)$r.squared

regML2 <- ggplot(ML2, aes(x = MeanLAI_9014, y = n)) +
  geom_point () +
  theme_light() +
  labs(x = "MeanLAI_9014", 
       y = "Nombre d'adhésions à des associations de municipalités") +
  geom_abline(intercept = reg2$coefficients[1], slope = reg2$coefficients[2], color="#E69F00",
              linetype = "dashed", size = 1.5) +
  annotate(geom = "text", x = 15, y = 1000, label = paste0("R2 = ", 
                                                           round(summary(reg2)$r.squared, 2))) 
regML2
