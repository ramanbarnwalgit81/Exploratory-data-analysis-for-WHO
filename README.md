# 🌍 WHO Global Health Data — Multivariate EDA

A comprehensive multivariate exploratory data analysis of **World Health Organization (WHO) Life Expectancy** data across 193 countries, covering 22 health and development attributes.

> **Authors:** Barnwal, Caldwell, Mukut  
> **Data Source:** [Kaggle — WHO Life Expectancy Dataset](https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who)

---

## 📁 Project Structure

```
who-eda-project/
├── data/
│   ├── raw/                   # Original CSV from Kaggle
│   └── processed/             # Cleaned & aggregated data
├── R/
│   ├── 01_data_cleaning.R     # NA handling, imputation, aggregation
│   ├── 02_visualizations.R    # Histograms, boxplots, scatterplots
│   ├── 03_pca.R               # Principal Component Analysis
│   ├── 04_cca.R               # Canonical Correlation Analysis
│   ├── 05_mds_ca.R            # MDS & Correspondence Analysis
│   ├── 06_factor_analysis.R   # EFA & CFA
│   └── 07_clustering.R        # Hierarchical, K-means, Model-based
├── outputs/
│   └── figures/               # All generated plots (PNG/PDF)
├── who_eda.Rproj              # RStudio project file
├── install_packages.R         # One-shot dependency installer
└── README.md
```

---

## 📊 Dataset Overview

| Attribute | Description |
|---|---|
| `Country` | Name of country |
| `Year` | Survey year (2000–2015) |
| `Status` | Developed / Developing |
| `Life_Exp` | Life expectancy (years) |
| `Adult_Mort` | Adult mortality rate (per 1000) |
| `Alcohol` | Per capita alcohol consumption (litres) |
| `BMI` | Average population BMI |
| `Polio` | Polio immunization coverage (%) |
| `HIV_AIDS` | HIV/AIDS deaths per 1000 live births |
| `GDP` | GDP per capita (USD) |
| `Schooling` | Average years of schooling |
| `Inc_Comp_Res` | Human Development Index (income) |
| ... | (22 total attributes) |

---

## 🔬 Analysis Pipeline

### 1. Data Cleaning (`01_data_cleaning.R`)
- Handle missing values with defaults + median imputation
- Aggregate time series → cross-sectional (country-level means)
- Merge external GDP & Population from WHO
- Remove 12 countries with insufficient data
- Drop redundant/collinear features

### 2. Initial Visualizations (`02_visualizations.R`)
- Correlation heatmaps (before & after aggregation)
- Univariate histograms and boxplots for all variables
- Bivariate boxplots: BMI, Adult Mortality, GDP, HIV vs. Life Expectancy
- Bubble plot, 3D scatterplot, KDE pairs plot
- QQ plots (univariate & multivariate normality)

### 3. Dimension Reduction (`03_pca.R`, `04_cca.R`, `05_mds_ca.R`)
- **PCA** — 10 components; PC1 explains ~53% variance; PC regression (R² = 0.90)
- **CCA** — Canonical correlations: 0.848, 0.448, 0.278 across variable sets
- **MDS** — Classical MDS on Euclidean distances; variable configuration map
- **Correspondence Analysis** — GDP quintile × BMI category contingency (χ² = 98, p < 1e-15)

### 4. Factor Analysis (`06_factor_analysis.R`)
- **EFA** (promax rotation, 2 factors): Factor1 = Development, Factor2 = Mortality
- **CFA** (SEM): GFI = 0.81; AGFI = 0.69; SRMR = 0.15

### 5. Clustering (`07_clustering.R`)
- **Hierarchical** (complete linkage, k=3): Advanced / Developing / Under-developed world map
- **K-means** (k=3): PCA biplot with geographic choropleth map
- **Model-based** (Mclust): Probabilistic 3-cluster solution

---

## ⚙️ Setup & Usage

### Prerequisites
- R ≥ 4.2
- RStudio (recommended)

### Install Dependencies
```r
source("install_packages.R")
```

### Run Full Analysis
```r
source("R/01_data_cleaning.R")
source("R/02_visualizations.R")
source("R/03_pca.R")
source("R/04_cca.R")
source("R/05_mds_ca.R")
source("R/06_factor_analysis.R")
source("R/07_clustering.R")
```

Or open `who_eda.Rproj` in RStudio and run scripts interactively.

---

## 📈 Key Findings

- **GDP, Income Composition, and BMI** are the strongest positive predictors of life expectancy
- **HIV/AIDS and Adult Mortality** are the dominant negative predictors (Factor 2 in EFA)
- **PC1** (Overall Development) explains 53% of variance; **PC2** (Epidemiology) explains 14%
- K-means and hierarchical clustering both recover a clean 3-group structure aligned with known development status
- Developed nations cluster tightly; underdeveloped nations (primarily sub-Saharan Africa) form a distinct low-expectancy group

---

## 📦 R Package Dependencies

| Package | Purpose |
|---|---|
| `tidyverse` | Data wrangling & ggplot2 |
| `corrplot` | Correlation heatmaps |
| `ggplot2` | Publication-quality plots |
| `CCA` | Canonical correlation analysis |
| `lavaan` | Confirmatory factor analysis (SEM) |
| `psych` | Exploratory factor analysis |
| `mclust` | Model-based clustering |
| `maps` / `ggmap` | World choropleth maps |
| `MVN` | Multivariate normality tests |
| `FactoMineR` | Correspondence analysis |
| `scatterplot3d` | 3D plots |

---

## 📄 License

This project is for academic/educational use. Data sourced from [WHO](https://www.who.int/) via Kaggle.
