# =============================================================================
# 04_cca.R
# WHO Life Expectancy — Canonical Correlation Analysis
# =============================================================================

library(tidyverse)
library(CCA)
library(corrplot)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# Variable partitioning
x_vars <- c("Status", "BMI", "Total_Exp", "GDP", "Inc_Comp_Res", "Alcohol", "Polio")
y_vars <- c("Adult_Mort", "HIV_AIDS", "Thin_1_19")

df_cc <- na.omit(life_exp[, c(x_vars, y_vars)])
X <- df_cc[, x_vars]
Y <- df_cc[, y_vars]

# =============================================================================
# 1. Run CCA
# =============================================================================
cat("Running Canonical Correlation Analysis...\n")

cca_result <- cc(X, Y)
cat("\nCanonical correlations:\n")
print(round(cca_result$cor, 3))

# =============================================================================
# 2. Scree Plot
# =============================================================================
png("outputs/figures/14_cca_scree.png", width = 800, height = 600, res = 120)
plot(cca_result$cor,
     type = "b",
     pch  = 19,
     col  = "#4575b4",
     xlab = "Index",
     ylab = "Canonical Correlation",
     main = "CCA Scree Plot",
     ylim = c(0, 1))
abline(h = 0.5, lty = 2, col = "gray50")
dev.off()

# =============================================================================
# 3. Correlation Heatmaps (X, Y, Cross)
# =============================================================================
Xcor <- cor(X)
Ycor <- cor(Y)

png("outputs/figures/15_cca_x_heatmap.png", width = 800, height = 700, res = 120)
corrplot(Xcor, method = "color", type = "full",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "X Correlation", mar = c(0, 0, 2, 0),
         tl.cex = 0.8, number.cex = 0.6, addCoef.col = "black")
dev.off()

png("outputs/figures/16_cca_y_heatmap.png", width = 700, height = 600, res = 120)
corrplot(Ycor, method = "color", type = "full",
         col = colorRampPalette(c("blue", "white", "red"))(200),
         title = "Y Correlation", mar = c(0, 0, 2, 0),
         tl.cex = 0.9, number.cex = 0.7, addCoef.col = "black")
dev.off()

# =============================================================================
# 4. Canonical Variable Scatter Plot
# =============================================================================
U <- as.matrix(X) %*% cca_result$xcoef[, 1]
V <- as.matrix(Y) %*% cca_result$ycoef[, 1]

png("outputs/figures/17_cca_canonical_scatter.png", width = 800, height = 700, res = 120)
plot(U, V,
     xlab = "U[,1]", ylab = "V[,1]",
     main = "The pair of canonical variables",
     pch  = 19, col  = "#4575b4", cex = 0.7)
abline(lm(V ~ U), col = "#d73027", lwd = 2)
dev.off()

cat("\n✓ CCA complete.\n")
