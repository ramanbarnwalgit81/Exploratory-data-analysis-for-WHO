# Data Directory

## Raw Data

Place the Kaggle dataset here:

```
data/raw/life_expectancy_data.csv
```

Download from: https://www.kaggle.com/datasets/kumarajarshi/life-expectancy-who

The raw CSV is not committed to this repository (see `.gitignore`).

## Processed Data

After running `R/01_data_cleaning.R`, the cleaned file will be written to:

```
data/processed/life_exp_clean.csv
```

This file contains one row per country (181 countries after omissions),
with ~12 variables aggregated from the 2000–2015 time series.
