# =============================================================================
# 03_pca.R
# WHO Life Expectancy — Principal Component Analysis
# =============================================================================

library(tidyverse)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

numeric_vars <- setdiff(names(life_exp)[sapply(life_exp, is.numeric)], "Life_Exp")
df_pca <- na.omit(life_exp[, c("Country", "Life_Exp", numeric_vars)])

# =============================================================================
# 1. Run PCA (scaled)
# =============================================================================
cat("Running PCA...\n")

pca_result <- princomp(df_pca[, numeric_vars], cor = TRUE)

cat("\nImportance of components:\n")
print(summary(pca_result))

cat("\nLoadings:\n")
print(loadings(pca_result))

# =============================================================================
# 2. Cumulative Variance Scree Plot
# =============================================================================
var_explained <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
cum_var       <- cumsum(var_explained) * 100

png("outputs/figures/11_pca_scree.png", width = 900, height = 700, res = 120)
plot(cum_var,
     type  = "b",
     pch   = 19,
     col   = "#4575b4",
     xlab  = "Principle Component",
     ylab  = "Cumulative % of Variance Explained",
     main  = "Cumulative Variance Scree Plot (princomp)",
     ylim  = c(50, 100))
abline(h = 80, lty = 2, col = "gray50")
dev.off()

# =============================================================================
# 3. Biplot (PC1 vs PC2)
# =============================================================================
png("outputs/figures/12_pca_biplot.png", width = 1200, height = 1000, res = 120)
biplot(pca_result,
       cex    = c(0.4, 0.9),
       col    = c("black", "red"),
       main   = "PCA Biplot (PC1 vs PC2)",
       expand = 1.2)
dev.off()

# =============================================================================
# 4. PC Regression on Life_Exp
# =============================================================================
cat("\nFitting PC regression (Life_Exp ~ PC1 + PC2 + PC3 + PC4)...\n")

scores <- as.data.frame(pca_result$scores)

# Interpret PCs based on loadings
names(scores)[1] <- "PC1_Overall"
names(scores)[2] <- "PC2_Epidemiology"
names(scores)[3] <- "PC3_Nutrition_Immunization"
names(scores)[4] <- "PC4_Healthcare"

pc_df <- cbind(Life_Exp = df_pca$Life_Exp, scores[, 1:4])

pc_model <- lm(Life_Exp ~ PC1_Overall + PC2_Epidemiology +
                 PC3_Nutrition_Immunization + PC4_Healthcare,
               data = pc_df)

cat("\nPC Regression Summary:\n")
print(summary(pc_model))

# =============================================================================
# 5. Explained Variance Bar Plot
# =============================================================================
png("outputs/figures/13_pca_variance_bar.png", width = 900, height = 600, res = 120)
barplot(var_explained[1:10] * 100,
        names.arg = paste0("PC", 1:10),
        col       = "#4575b4",
        border    = NA,
        ylab      = "% Variance Explained",
        main      = "PCA — Individual Variance per Component",
        las       = 2)
dev.off()

cat("\n✓ PCA complete. Figures saved to outputs/figures/\n")
