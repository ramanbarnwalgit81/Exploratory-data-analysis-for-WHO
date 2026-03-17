# =============================================================================
# 05_mds_ca.R
# WHO Life Expectancy — MDS & Correspondence Analysis
# =============================================================================

library(tidyverse)
library(FactoMineR)
library(factoextra)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

indep_vars <- c("Status", "BMI", "Total_Exp", "GDP", "Inc_Comp_Res",
                "Adult_Mort", "Alcohol", "Polio", "HIV_AIDS", "Thin_1_19")

# =============================================================================
# A. MULTIDIMENSIONAL SCALING (MDS)
# =============================================================================
cat("Running Classical MDS...\n")

mds_vars <- life_exp[, indep_vars]
mds_vars <- na.omit(mds_vars)
country_names <- life_exp$Country[complete.cases(life_exp[, indep_vars])]

d   <- dist(mds_vars)
mds <- cmdscale(d)

png("outputs/figures/18_mds_classical.png", width = 1100, height = 900, res = 110)
plot(mds, pch = ".", col = "white",
     main = "Classical MDS — Countries",
     xlab = "mds[,1]", ylab = "mds[,2]")
text(mds[, 1], mds[, 2],
     labels = abbreviate(country_names),
     cex = 0.45, col = "#d73027")
dev.off()

# MDS Eigenvalue Plot
mds2 <- cmdscale(d, eig = TRUE)
png("outputs/figures/19_mds_eigenvalues.png", width = 900, height = 600, res = 120)
plot(mds2$eig, type = "b", pch = 19,
     main = "MDS Eigenvalue Plot",
     xlab = "Index", ylab = "Eigenvalue")
dev.off()

# =============================================================================
# B. MDS ON CORRELATIONS (Variable Configuration)
# =============================================================================
cat("Running MDS on correlation matrix...\n")

df_num <- na.omit(life_exp[, indep_vars])
cor_mat   <- cor(df_num)
dist_vars <- as.dist(1 - abs(cor_mat))
mds_vars_result <- cmdscale(dist_vars, eig = TRUE)

png("outputs/figures/20_mds_variable_config.png", width = 1000, height = 800, res = 120)
plot(mds_vars_result$points[, 1], mds_vars_result$points[, 2],
     xlab = "Dimension 1", ylab = "Dimension 2",
     main = "MDS on Correlations — Variable Configuration",
     pch  = 19, col = "#4575b4", cex = 1.2)
text(mds_vars_result$points[, 1], mds_vars_result$points[, 2],
     labels = rownames(mds_vars_result$points),
     pos = 3, cex = 0.75, col = "#555555")
abline(h = 0, v = 0, lty = 2, col = "gray70")
dev.off()

# =============================================================================
# C. CORRESPONDENCE ANALYSIS (CA)
# =============================================================================
cat("\nRunning Correspondence Analysis...\n")

# Create GDP quintile categories
life_exp$GDP_cat <- cut(life_exp$GDP,
                        breaks = 5,
                        labels = c("Subsistence", "Growing", "Stable",
                                   "Prosperous", "Affluent"),
                        include.lowest = TRUE)

# WHO-standard BMI categories
life_exp$BMI_cat <- cut(life_exp$BMI,
                        breaks = c(-Inf, 18.5, 25, 30, Inf),
                        labels = c("Under", "Normal", "Over", "Obese"),
                        right  = FALSE)

tbl <- table(life_exp$GDP_cat, life_exp$BMI_cat)
cat("\nContingency Table (GDP category × BMI category):\n")
print(tbl)

cat("\nChi-squared Test:\n")
print(chisq.test(tbl))

# Run CA
ca_result <- CA(tbl, graph = FALSE)

png("outputs/figures/21_correspondence_analysis.png", width = 1000, height = 800, res = 120)
fviz_ca_biplot(ca_result,
               repel     = TRUE,
               title     = "Correspondence Analysis: GDP Category × BMI Category",
               col.row   = "#4575b4",
               col.col   = "#d73027") +
  theme_minimal()
dev.off()

cat("\n✓ MDS & CA complete.\n")
