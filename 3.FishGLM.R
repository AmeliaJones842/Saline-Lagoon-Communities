# SALINE LAGOONS PROJECT

# AIM: STATISTICALLY DETERMINE WHICH ENVIRONMENTAL VARIABLES IMPACT FISH COMUNITIES

# ANALYSIS 2: USING GENERAL LINEAR MODELS TO DETERMINE ENVIRONMENTAL IMPACT ON FISH ABUNDANCE AND DIVERSITY

###
rm(list=ls())
graphics.off()
###

# LOAD PACKAGES
library(MuMIn)
library(effects)
library(car)
###

# IMPORT DATA
SALINE <- read.csv("Saline_lagoon.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
SALINE <- na.omit(SALINE)
###

# DATA FORMATTING
FISH.sum <- apply(SALINE[,c("G.spp","M.spp","TLGM", "So", "GGM", "FF", "EF", "TSS", "EE", "H")], 1, sum)
SALINE$FISH <- FISH.sum
T.FISH <- sqrt(SALINE$FISH)
SALINE$T.FISH <- T.FISH
fish <- SALINE[c("G.spp","M.spp","TLGM", "So", "GGM", "FF", "EF", "TSS", "EE", "H")]
fish.shan <- diversity(fish, index = "shannon")
SALINE$FISH.shan <- fish.shan
SAMPLE <- SALINE[,1:4]
WATER <- SALINE[,5:10]
WATER.STZ <- scale(WATER)
###

# FISH ABUNDANCE GENERAL LINEAR MODELS
Fish.mod.comp1 <- dredge(glm(T.FISH ~ Temp + DO + PH + Salinity + Turbidity + Chlor, data = SALINE, family=Gamma(link = "inverse"), na.action = na.fail))
Fishabun.mod <- get.models(Fish.mod.comp1, subset = 1)[[1]]
summary(Fishabun.mod)
###

# FISH DIVERSITY GENERAL LINEAR MODELS
Fish.mod.comp2 <- dredge(glm(FISH.shan ~ Temp + DO + PH + Salinity + Turbidity + Chlor, data = SALINE, family=gaussian, na.action = na.fail))
Fishdiv.mod <- get.models(Fish.mod.comp2, subset = 1)[[1]]
summary(Fishdiv.mod)
###


# PLOT FOR FISH ABUN DO AND FISH DIV TEMP
par(mfrow = c(1,2), oma = c(5,0,0,0), mar = c(4,4,2,0.2), xpd = FALSE)
plot(SALINE$DO, SALINE$T.FISH, pch = 16, las = 1, cex.lab = 1.1, xlab = "Dissolved Oxygen (mg/l) ***", ylab = "Fish abundance (sq root)")
DO.PE1 <- effect("DO", Fishabun.mod, partial_residuals = TRUE)
lines(DO.PE1$x$DO, as.vector(DO.PE1$fit), col = "#669999", lwd = 2, lty = 1)
plot(SALINE$Temp, SALINE$FISH.shan, pch = 16, las = 1, xlab = "Temperature (C) **", ylab = "Fish diversity (Shannons index)", col = "#000000")
Temp.PE2 <- effect("Temp", Fishdiv.mod, partial_residuals = TRUE)
lines(Temp.PE2$x$Temp, as.vector(Temp.PE2$fit), col = "#990000", lwd = 2, lty = 1)

# EXTRA STATS

# ABUN ~ DO
Xvals <- DO.PE1$x$DO
Yvals <-as.vector(DO.PE1$fit)
COEF <- coef(Fishabun.mod)
COEF <- COEF["DO"]
intvals <- (Yvals/Xvals) - COEF
Intercept <- mean(intvals)
Lowval <- Intercept + (COEF * 3)
Highval <- Intercept + (COEF * 8)
percchange <- ((Highval - Lowval) / Lowval) * 100
percchange
###

# DIV ~ TEMP
Xvals <- Temp.PE2$x$Temp
Yvals <-as.vector(Temp.PE2$fit)
COEF <- coef(Fishdiv.mod)
COEF <- COEF["Temp"]
intvals <- (Yvals/Xvals) - COEF
Intercept <- mean(intvals)
Lowval <- Intercept + (COEF * 10)
Highval <- Intercept + (COEF * 15)
percchange <- ((Highval - Lowval) / Lowval) * 100
percchange
###

# FISH ABUN ~ FISH DIV COR
cor.test(SALINE$FISH, SALINE$FISH.shan)
plot(SALINE$FISH, SALINE$FISH.shan, pch = 16)
fit <- lm(SALINE$FISH.shan ~SALINE$FISH)
abline(fit, col = "red")
###

cor.test(SALINE$DO, SALINE$Chlor)
plot(SALINE$DO, SALINE$Chlor, pch = 16)
fit <- lm(SALINE$DO ~SALINE$Chlor)
abline(fit, col = "red")
###
