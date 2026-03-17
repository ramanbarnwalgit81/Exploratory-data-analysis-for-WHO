# install_packages.R
# Run this once to install all required packages

packages <- c(
  "tidyverse",
  "corrplot",
  "ggplot2",
  "reshape2",
  "CCA",
  "lavaan",
  "semPlot",
  "psych",
  "mclust",
  "maps",
  "ggmap",
  "MVN",
  "FactoMineR",
  "factoextra",
  "scatterplot3d",
  "GGally",
  "MASS",
  "cluster",
  "mice",
  "Hmisc",
  "readr"
)

installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]

if (length(to_install) > 0) {
  cat("Installing", length(to_install), "packages...\n")
  install.packages(to_install, dependencies = TRUE)
} else {
  cat("All packages already installed.\n")
}

cat("Loading packages to verify...\n")
invisible(lapply(packages, library, character.only = TRUE))
cat("✓ All packages loaded successfully.\n")
