# =============================================================================
# 02_visualizations.R
# WHO Life Expectancy — Initial Visualizations
# =============================================================================

library(tidyverse)
library(corrplot)
library(GGally)
library(scatterplot3d)
library(MASS)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
numeric_vars <- names(life_exp)[sapply(life_exp, is.numeric)]

# =============================================================================
# A. CORRELATION HEATMAP
# =============================================================================
cat("Plotting correlation heatmap...\n")

cor_matrix <- cor(life_exp[, numeric_vars], use = "complete.obs")

png("outputs/figures/01_correlation_heatmap.png", width = 1200, height = 1000, res = 120)
corrplot(cor_matrix,
         method   = "color",
         type     = "lower",
         addCoef.col = "black",
         number.cex = 0.6,
         tl.cex   = 0.8,
         tl.col   = "black",
         col      = colorRampPalette(c("#d73027", "white", "#4575b4"))(200),
         title    = "Correlation Matrix — WHO Life Expectancy",
         mar      = c(0, 0, 2, 0))
dev.off()

# =============================================================================
# B. UNIVARIATE HISTOGRAMS
# =============================================================================
cat("Plotting univariate histograms...\n")

png("outputs/figures/02_uv_histograms.png", width = 1800, height = 1200, res = 120)
par(mfrow = c(3, 4), mar = c(4, 4, 3, 1))
for (v in numeric_vars) {
  hist(life_exp[[v]],
       main  = paste("Hist:", v),
       xlab  = v,
       col   = "#4575b4",
       border = "white",
       breaks = 20)
}
dev.off()

# =============================================================================
# C. UNIVARIATE BOXPLOTS
# =============================================================================
cat("Plotting univariate boxplots...\n")

png("outputs/figures/03_uv_boxplots.png", width = 1800, height = 1200, res = 120)
par(mfrow = c(3, 4), mar = c(4, 4, 3, 1))
for (v in numeric_vars) {
  boxplot(life_exp[[v]],
          main  = paste("Boxplot:", v),
          ylab  = v,
          col   = "#4575b4",
          border = "black",
          outline = TRUE)
}
dev.off()

# =============================================================================
# D. BIVARIATE BOXPLOTS (vs Life_Exp)
# =============================================================================
bv_vars <- c("BMI", "Adult_Mort", "GDP", "HIV_AIDS")

for (v in bv_vars) {
  fname <- paste0("outputs/figures/04_bv_", tolower(v), "_vs_lifeexp.png")
  png(fname, width = 1000, height = 800, res = 120)

  plot(life_exp$Life_Exp, life_exp[[v]],
       xlab  = "Life_Exp",
       ylab  = v,
       main  = paste("Bivariate Boxplot:", v, "vs Life_Exp"),
       pch   = 20,
       col   = ifelse(life_exp$Status == 1, "#d73027", "#4575b4"),
       cex   = 0.8)

  # Add ellipse
  tryCatch({
    df_tmp <- na.omit(data.frame(x = life_exp$Life_Exp, y = life_exp[[v]]))
    mu <- colMeans(df_tmp)
    sigma <- cov(df_tmp)
    ellipse_pts <- function(mu, sigma, level = 0.95, n = 100) {
      theta <- seq(0, 2 * pi, length.out = n)
      circle <- cbind(cos(theta), sin(theta))
      eigv   <- eigen(sigma)
      r      <- sqrt(qchisq(level, 2))
      pts    <- t(mu + r * eigv$vectors %*% diag(sqrt(eigv$values)) %*% t(circle))
      as.data.frame(pts)
    }
    for (lev in c(0.5, 0.95, 0.99)) {
      ell <- ellipse_pts(mu, sigma, lev)
      lines(ell[, 1], ell[, 2], lty = 2, col = "gray40")
    }
  }, error = function(e) NULL)

  text(life_exp$Life_Exp, life_exp[[v]],
       labels = life_exp$Country,
       cex    = 0.35,
       col    = ifelse(life_exp$Status == 1, "#d73027", "black"),
       pos    = 3)

  legend("topleft",
         legend = c("Developed", "Developing"),
         col    = c("#d73027", "#4575b4"),
         pch    = 20)
  dev.off()
  cat("  Saved:", fname, "\n")
}

# =============================================================================
# E. BUBBLE PLOT (Life_Exp vs GDP, bubble size = BMI)
# =============================================================================
cat("Plotting bubble plot...\n")

png("outputs/figures/05_bubble_plot.png", width = 1200, height = 900, res = 120)
symbols(x     = life_exp$Life_Exp,
        y     = life_exp$GDP,
        circles = sqrt(life_exp$BMI),
        inches = 0.15,
        ann    = FALSE,
        bg     = ifelse(life_exp$Status == 1, "#4575b4", "#d73027"),
        fg     = "black")
title(main = "GDP vs Life Expectancy\n(bubble = BMI)",
      xlab = "Life Expectancy",
      ylab = "GDP (log scale)")
text(life_exp$Life_Exp, life_exp$GDP,
     labels = life_exp$Country, cex = 0.35, col = "#d73027")
dev.off()

# =============================================================================
# F. 3D SCATTERPLOT (Life_Exp, BMI, GDP)
# =============================================================================
cat("Plotting 3D scatterplot...\n")

png("outputs/figures/06_3d_scatterplot.png", width = 1100, height = 900, res = 120)
scatterplot3d(
  x     = life_exp$Life_Exp,
  y     = life_exp$BMI,
  z     = life_exp$GDP,
  color = ifelse(life_exp$Status == 1, "#4575b4", "#d73027"),
  pch   = 19,
  cex.symbols = 0.6,
  main  = "Life Expectancy vs BMI vs GDP (3D)",
  xlab  = "Life Expectancy",
  ylab  = "BMI",
  zlab  = "GDP"
)
dev.off()

# =============================================================================
# G. QQ PLOTS (univariate normality)
# =============================================================================
cat("Plotting QQ plots...\n")

qq_vars <- c("Life_Exp", "GDP", "Population" = "Adult_Mort")

png("outputs/figures/07_qqplot_lifeexp.png", width = 900, height = 750, res = 120)
qqnorm(life_exp$Life_Exp, main = "Q-Q Plot: Life Expectancy", pch = 1)
qqline(life_exp$Life_Exp, col = "steelblue", lwd = 2)
dev.off()

png("outputs/figures/08_qqplot_gdp.png", width = 900, height = 750, res = 120)
qqnorm(life_exp$GDP, main = "Q-Q Plot: GDP", pch = 1)
qqline(life_exp$GDP, col = "steelblue", lwd = 2)
dev.off()

# =============================================================================
# H. MULTIVARIATE NORMALITY QQ PLOT
# =============================================================================
cat("Plotting multivariate normality QQ plot...\n")

df_mv <- na.omit(life_exp[, numeric_vars])
mu    <- colMeans(df_mv)
S     <- cov(df_mv)
d2    <- mahalanobis(df_mv, mu, S)
chi_q <- qchisq(ppoints(nrow(df_mv)), df = ncol(df_mv))

png("outputs/figures/09_mv_normality_qqplot.png", width = 900, height = 750, res = 120)
plot(chi_q, sort(d2),
     xlab = expression(chi^2 ~ "Quantile"),
     ylab = "Ordered Squared Mahalanobis Distances",
     main = "Multivariate Normality QQ Plot",
     pch  = 20,
     cex  = 0.7)
abline(0, 1, col = "steelblue", lwd = 2)
# Label top outliers
top_idx <- order(d2, decreasing = TRUE)[1:5]
text(chi_q[order(d2)[nrow(df_mv) - 4:0]],
     sort(d2)[nrow(df_mv) - 4:0],
     labels = life_exp$Country[top_idx],
     cex = 0.6, pos = 2)
dev.off()

# =============================================================================
# I. KDE PAIRS PLOT
# =============================================================================
cat("Plotting KDE pairs...\n")

png("outputs/figures/10_kde_pairs.png", width = 1800, height = 1600, res = 100)
pairs(life_exp[, numeric_vars[1:8]],
      upper.panel = function(x, y, ...) {
        r <- round(cor(x, y, use = "complete.obs"), 3)
        txt <- as.character(r)
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(0, 1, 0, 1))
        text(0.5, 0.5, txt, cex = abs(r) * 2 + 0.5,
             col = ifelse(r > 0, "#d73027", "#4575b4"))
      },
      diag.panel = function(x, ...) {
        usr <- par("usr"); on.exit(par(usr))
        par(usr = c(usr[1:2], 0, 1.5))
        d <- density(x, na.rm = TRUE)
        d$y <- d$y / max(d$y)
        polygon(d$x, d$y, col = "#74add1", border = NA)
      },
      lower.panel = function(x, y, ...) {
        points(x, y, pch = 20, cex = 0.5, col = "gray40")
      }
)
dev.off()

cat("\n✓ All visualizations saved to outputs/figures/\n")
