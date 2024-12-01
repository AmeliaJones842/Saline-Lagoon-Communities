# SALINE LAGOONS PROJECT

# AIM: STATISTICALLY DETERMINE WHICH ENVIRONMENTAL VARIABLES IMPACT INVERTEBRATE COMUNITIES

# ANALYSIS 2: USING GENERAL LINEAR MODELS TO DETERMINE ENVIRONMENTAL IMPACT ON INVERTEBRATE ABUNDANCE AND DIVERSITY

###
rm(list=ls())
graphics.off()
###

# LOAD PACKAGES
library(MuMIn)
library(effects)
###

# IMPORT DATA
SALINE <- read.csv("Saline_lagoon.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
SALINE <- na.omit(SALINE)
###

# DATA FORMATTING
INV.sum <- apply(SALINE[,c("Am","CFL", "EGC", "EMS", "C.spp", "BW", "P")], 1, sum)
SALINE$INV <- INV.sum
T.INV <- sqrt(SALINE$INV)
SALINE$T.INV <- T.INV
T.SHRIMP <-sqrt(SALINE$S.spp)
SALINE$T.SHRIMP <-T.SHRIMP
INV.data <- SALINE[c("Am","CFL", "EGC", "EMS", "C.spp", "BW", "P")]
INV.shan <- diversity(INV.data, index = "shannon")
SALINE$INV.shan <- INV.shan
SAMPLE <- SALINE[,1:4]
WATER <- SALINE[,5:10]
WATER.STZ <- scale(WATER)
###

# INVERTEBRATE ABUNDANCE GENERAL LINEAR MODELS
Inv.mod.comp1 <- dredge(glm(T.INV ~ Temp + DO + PH + Salinity + Turbidity + Chlor, data = SALINE, family=gaussian, na.action = na.fail))
Invabun.mod <- get.models(Inv.mod.comp1, subset = 2)[[1]]
summary(Invabun.mod)
###

# INVERTEBRATE DIVERSITY GENERAL LINEAR MODELS
Inv.mod.comp2 <- dredge(glm(INV.shan ~ Temp + DO + PH + Salinity + Turbidity + Chlor, data = SALINE, family=gaussian, na.action = na.fail))
Invdiv.mod <- get.models(Inv.mod.comp2, subset = 1)[[1]]
summary(Invdiv.mod)
###

# ABUN ~ TEMP
TEMP.PE1 <- effect("Temp", Invabun.mod, partial_residuals = TRUE)
Xvals <- TEMP.PE1$x$Temp
Yvals <-as.vector(TEMP.PE1$fit)
COEF <- coef(Invabun.mod)
COEF <- COEF["Temp"]
intvals <- (Yvals/Xvals) - COEF
Intercept <- mean(intvals)
Lowval <- Intercept + (COEF * 10)
Highval <- Intercept + (COEF * 15)
percchange <- ((Highval - Lowval) / Lowval) * 100
percchange
