# SALINE LAGOONS PROJECT

# AIM: STATISTICALLY DETERMINE WHICH ENVIRONMENTAL VARIABLES IMPACT COMMUNITY COMPOSITION

# ANALYSIS 2: USING REDUNDANCY ANALYSIS TO  

###

rm(list=ls())
graphics.off()
dir()

###

# LOAD PACKAGES

library(vegan)
library(ggord)

# IMPORT DATA

SALINE <- read.csv("Saline_lagoon.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)
SALINE <- na.omit(SALINE)

T.ABUN <- decostand(SALINE[,11:28], method = "hellinger")
SALINE[, 11:28] <- T.ABUN

WATER <- SALINE[,5:10]
T.WATER <- decostand(WATER, method = "hellinger")
WATER.DF <- as.data.frame(T.WATER)

SAMPLE <- SALINE[,1:4]

###

# REDUNDANCY ANALYSIS

# CREATE ALL POSSIBLE MODELS
predictors <- c("Temp", "DO", "PH", "Salinity", "Turbidity", "Chlor")
combinations <- unlist(lapply(1:length(predictors), function(i) combn(predictors, i, simplify = FALSE)), recursive = FALSE)
model_results <- data.frame(Model = character(), Adjusted_R2 = numeric(), stringsAsFactors = FALSE)
fit_rda_model <- function(predictor_combination, response_var, data) {
  formula_str <- paste(response_var, "~", paste(predictor_combination, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  model <- rda(formula_obj, data = data)
  adj_r2 <- RsquareAdj(model)$adj.r.squared
  return(list(formula = formula_str, adj_r2 = adj_r2))
}

# FIT MODELS AND FIND BEST FIT USING R2
for (i in seq_along(combinations)) {
  result <- fit_rda_model(combinations[[i]], "T.ABUN", WATER.DF)
  model_results[i, ] <- list(Model = result$formula, Adjusted_R2 = result$adj_r2)
}
model_results <- model_results[order(-model_results$Adjusted_R2), ]
print(model_results[1, ])

# TEST RDA ASSUMPTIONS
RDA <- rda(T.ABUN ~ Temp + DO + PH, data = WATER.DF)
shapiro.test(residuals(RDA))                                                    # Residuals not normally distributed

# TEST DB-RDA ASSUMPTIONS
Dist.matrix <- vegdist(T.ABUN, method = "bray")
db_rda <- capscale(Dist.matrix ~ Temp + DO + PH, data = WATER.DF)

# LOOK AT RESULTS
summary(db_rda)
anova.cca(db_rda, by= "axis")
anova.cca(db_rda, by= "terms")

# PLOT RESULTS
new.m.o <- c("February", "March", "April", "May", "July", "August", "September")
Month <- factor(SAMPLE$Month, levels = new.m.o)
CO <- c("#0033CC", "#99CCFF", "#336633", "#99CC66", "#993300",  "#FF9933","#CC99FF")
SHAP <- c(1, 16, 0, 15, 2, 18, 6)
SHAP <- c(16, 16, 15, 15, 18, 18, 17)

Scores <- scores(db_rda, display = "sites")
par(mfrow = c(1,1), oma = c(0,0,0,6), mar = c(4,4,1,2))
plot(Scores[, 1], Scores[, 2], col = CO[Month], pch = SHAP[Month], 
     cex = 1.5, las = 1, cex.lab = 1.2, xlab = "db-RDA1 (85.16%)", ylab = "db-RDA2 (14.84%)")

ordiellipse(db_rda, Month, display = "sites", kind = "sd", col = CO,
            draw = "polygon", alpha = 30)
ordiellipse(db_rda, Month, display = "sites", kind = "sd", col = CO)

env_scores <- scores(db_rda, display = "bp")
env_scores_selected <- (env_scores[c("Temp", "DO", "PH"),] * 1.5)
arrows(0, 0, env_scores_selected[, 1], env_scores_selected[, 2], col = "black", length = 0.1)
text(env_scores_selected[, 1] * 1.1, env_scores_selected[, 2] * 1.1, 
     labels = rownames(env_scores_selected), col = "black", cex = 1)

par(xpd = NA)
legend("right",inset = -0.35, legend = new.m.o, fill = CO, pch = SHAP, cex = 1.2, ncol = 1, bty = "n",
       x.intersp = 0.3, y.intersp = 1.4)
