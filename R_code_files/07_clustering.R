# =============================================================================
# 07_clustering.R
# WHO Life Expectancy — Hierarchical, K-means & Model-based Clustering
# =============================================================================

library(tidyverse)
library(cluster)
library(mclust)
library(maps)
library(ggplot2)
library(factoextra)

life_exp <- read_csv("data/processed/life_exp_clean.csv")
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

cluster_vars <- c("BMI", "Total_Exp", "GDP", "Inc_Comp_Res",
                  "Alcohol", "Polio", "Adult_Mort", "HIV_AIDS", "Thin_1_19")

df_cluster <- na.omit(life_exp[, c("Country", "Status", cluster_vars)])
countries  <- df_cluster$Country

# Scale
life_exp_S   <- scale(df_cluster[, cluster_vars])
life_exp_S_D <- dist(life_exp_S)

cat("True cluster counts (Developed vs Developing):\n")
print(table(df_cluster$Status))

# =============================================================================
# A. HIERARCHICAL CLUSTERING
# =============================================================================
cat("\nRunning hierarchical clustering (complete linkage)...\n")

hc  <- hclust(life_exp_S_D, method = "complete")

# Scree plot
png("outputs/figures/25_hc_scree.png", width = 900, height = 600, res = 120)
plot(rev(hc$height), type = "b", pch = 19, col = "#4575b4",
     main = "HC Life Expectancy Scree Plot",
     xlab = "Index", ylab = "Height")
dev.off()

# Dendrogram
hcC <- cutree(hc, k = 3)
cat("Hierarchical cluster sizes:\n")
print(table(hcC))

png("outputs/figures/26_cluster_dendrogram.png", width = 1600, height = 900, res = 110)
plot(hc, labels = countries, cex = 0.35,
     main = "Cluster Dendrogram",
     xlab = "", ylab = "Height",
     hang = -1)
rect.hclust(hc, k = 3, border = c("#d73027", "#4575b4", "#1a9641"))
dev.off()

# =============================================================================
# B. K-MEANS CLUSTERING
# =============================================================================
cat("\nRunning K-means clustering (k=3)...\n")

set.seed(42)
km <- kmeans(life_exp_S, centers = 3, nstart = 25, iter.max = 100)

cat("K-means cluster sizes:", table(km$cluster), "\n")
cat("\nColumn Means for each cluster:\n")

cluster_means <- aggregate(life_exp_S, by = list(Cluster = km$cluster), FUN = mean)
print(round(cluster_means, 2))

# PCA Biplot with K-means clusters
pca_for_plot <- princomp(life_exp_S, cor = TRUE)
pc_scores    <- as.data.frame(pca_for_plot$scores[, 1:2])
pc_scores$Cluster <- factor(km$cluster,
                             levels = 1:3,
                             labels = c("Cluster 1", "Cluster 2", "Cluster 3"))
pc_scores$Country <- countries

# Map cluster labels to interpretable names
cluster_label_map <- c("1" = "Developing", "2" = "Advanced", "3" = "Under-developed")
# (order may differ; check means to assign labels properly)

png("outputs/figures/27_kmeans_pca_biplot.png", width = 1200, height = 900, res = 120)
ggplot(pc_scores, aes(x = Comp.1, y = Comp.2, color = Cluster, label = Country)) +
  geom_point(size = 1.5, alpha = 0.8) +
  geom_text(size = 1.8, vjust = -0.5, alpha = 0.7) +
  scale_color_manual(values = c("#4575b4", "#d73027", "#1a9641")) +
  labs(title = "PCA Biplot — K-means Clustering",
       x = "PC1", y = "PC2") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")
dev.off()

# =============================================================================
# C. MODEL-BASED CLUSTERING (Mclust)
# =============================================================================
cat("\nRunning Model-based clustering (Mclust)...\n")

mc <- Mclust(life_exp_S, G = 3)
cat("Mclust BIC-selected model:", mc$modelName, "\n")
cat("Cluster sizes:", table(mc$classification), "\n")

mclust_means <- aggregate(as.data.frame(life_exp_S),
                          by  = list(Cluster = mc$classification),
                          FUN = mean)

# Re-scale means back to original for interpretability
original_means <- aggregate(df_cluster[, cluster_vars],
                             by  = list(Cluster = mc$classification),
                             FUN = mean)
cat("\nCluster Means from Mclust (original scale):\n")
print(round(original_means, 1))

# =============================================================================
# D. WORLD MAPS
# =============================================================================
make_world_map <- function(country_vec, cluster_vec, labels_vec,
                           title_str, palette, out_path) {
  world <- map_data("world")

  lookup <- data.frame(
    Country = country_vec,
    Status  = factor(cluster_vec, labels = labels_vec),
    stringsAsFactors = FALSE
  )

  # Harmonize country names with map_data
  lookup$Country <- recode(lookup$Country,
    "United States of America" = "USA",
    "United Kingdom" = "UK",
    "Democratic Republic of the Congo" = "Democratic Republic of the Congo",
    "Congo" = "Republic of Congo"
  )

  map_df <- left_join(world, lookup, by = c("region" = "Country"))

  p <- ggplot(map_df, aes(x = long, y = lat, group = group, fill = Status)) +
    geom_polygon(color = "white", linewidth = 0.1) +
    scale_fill_manual(values = palette, na.value = "gray60",
                      na.translate = TRUE,
                      breaks = labels_vec,
                      labels = c(labels_vec, "NA")) +
    coord_fixed(1.3, ylim = c(-60, 90)) +
    labs(title = title_str, fill = "Status") +
    theme_void(base_size = 12) +
    theme(legend.position = "right",
          plot.title = element_text(hjust = 0.5, face = "bold"))

  ggsave(out_path, p, width = 14, height = 7, dpi = 130)
  cat("  Saved:", out_path, "\n")
}

# Original clusters (Developed/Developing)
orig_status <- ifelse(df_cluster$Status == 1, "Developed", "Developing")
make_world_map(countries, orig_status,
               c("Developed", "Developing"),
               "World Map: Original Clusters",
               c("Developed" = "#1a9641", "Developing" = "#d73027"),
               "outputs/figures/28_map_original_clusters.png")

# Hierarchical clusters
hc_labels <- c("1" = "Advanced", "2" = "Developing", "3" = "Under-developed")
make_world_map(countries, hc_labels[as.character(hcC)],
               c("Advanced", "Developing", "Under-developed"),
               "World Map: Hierarchical Clusters",
               c("Advanced" = "#1a9641", "Developing" = "#fdae61",
                 "Under-developed" = "#d73027"),
               "outputs/figures/29_map_hierarchical_clusters.png")

# K-means clusters
km_labels <- c("1" = "Developing", "2" = "Advanced", "3" = "Under-developed")
make_world_map(countries, km_labels[as.character(km$cluster)],
               c("Advanced", "Developing", "Under-developed"),
               "World Map: K-means Clusters",
               c("Advanced" = "#1a9641", "Developing" = "#fdae61",
                 "Under-developed" = "#d73027"),
               "outputs/figures/30_map_kmeans_clusters.png")

# Model-based clusters
mc_labels <- c("1" = "Under-developed", "2" = "Developing", "3" = "Advanced")
make_world_map(countries, mc_labels[as.character(mc$classification)],
               c("Advanced", "Developing", "Under-developed"),
               "World Map: Model-Based Clusters",
               c("Advanced" = "#1a9641", "Developing" = "#fdae61",
                 "Under-developed" = "#d73027"),
               "outputs/figures/31_map_modelbased_clusters.png")

cat("\n✓ Clustering complete. All figures saved to outputs/figures/\n")
