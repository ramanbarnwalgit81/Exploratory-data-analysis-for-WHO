# =============================================================================
# 06_factor_analysis.R
# WHO Life Expectancy — Exploratory & Confirmatory Factor Analysis
# =============================================================================

library(tidyverse)
library(psych)
library(lavaan)
library(semPlot)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

efa_vars <- c("Status", "BMI", "Total_Exp", "GDP", "Inc_Comp_Res",
              "Adult_Mort", "Alcohol", "Polio", "HIV_AIDS", "Thin_1_19")

df_fa <- na.omit(life_exp[, efa_vars])

# =============================================================================
# A. EXPLORATORY FACTOR ANALYSIS (EFA)
# =============================================================================
cat("Running EFA (2 factors, promax rotation)...\n")

efa_result <- factanal(x        = df_fa,
                       factors  = 2,
                       rotation = "promax")

cat("\nEFA Results:\n")
print(efa_result)

# Simplified loadings
loadings_df <- as.data.frame(unclass(efa_result$loadings))
loadings_df$Variable <- rownames(loadings_df)
loadings_df <- loadings_df %>%
  pivot_longer(cols = starts_with("Factor"),
               names_to = "Factor",
               values_to = "Loading")

# Factor loading heatmap
png("outputs/figures/22_efa_loadings.png", width = 800, height = 700, res = 120)
ggplot(loadings_df, aes(x = Factor, y = Variable, fill = Loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Loading, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#d73027", mid = "white", high = "#4575b4",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "EFA Factor Loadings (Promax Rotation)",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 10))
dev.off()

# =============================================================================
# B. DETERMINE NUMBER OF FACTORS — Scree
# =============================================================================
cat("\nDetermining number of factors via scree...\n")

ev <- eigen(cor(df_fa))$values

png("outputs/figures/23_efa_scree.png", width = 800, height = 600, res = 120)
plot(ev, type = "b", pch = 19, col = "#4575b4",
     xlab = "Factor Number", ylab = "Eigenvalue",
     main = "Scree Plot — Factor Analysis")
abline(h = 1, lty = 2, col = "#d73027")
legend("topright", legend = "Kaiser criterion (λ=1)",
       lty = 2, col = "#d73027", bty = "n")
dev.off()

# =============================================================================
# C. CONFIRMATORY FACTOR ANALYSIS (CFA)
# =============================================================================
cat("\nRunning CFA (2 latent factors: Development & Mortality)...\n")

who_model <- '
  # Latent factors
  Development =~ Status + BMI + Total_Exp + GDP + Inc_Comp_Res + Alcohol + Polio
  Mortality   =~ Adult_Mort + HIV_AIDS + Thin_1_19

  # Allow factor correlation
  Development ~~ Mortality
'

cfa_result <- sem(who_model,
                  data      = df_fa,
                  estimator = "ML")

cat("\nCFA Fit Indices:\n")
print(fitMeasures(cfa_result, c("chisq", "df", "pvalue", "gfi", "agfi", "srmr", "rmsea")))

cat("\nCFA Parameter Estimates:\n")
print(parameterEstimates(cfa_result, standardized = TRUE) %>%
        filter(op %in% c("=~", "~~")))

# Path Diagram
png("outputs/figures/24_cfa_path_diagram.png", width = 1100, height = 900, res = 120)
semPaths(cfa_result,
         what      = "std",
         layout    = "tree",
         edge.label.cex = 0.7,
         node.label.cex = 0.8,
         color     = list(lat = "#4575b4", man = "#74add1"),
         title     = TRUE,
         style     = "lisrel",
         residuals = TRUE,
         intercepts = FALSE)
title("CFA Path Diagram — Development & Mortality Factors")
dev.off()

cat("\n✓ Factor Analysis complete.\n")
