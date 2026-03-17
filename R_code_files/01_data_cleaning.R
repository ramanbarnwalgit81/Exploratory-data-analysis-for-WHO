# =============================================================================
# 01_data_cleaning.R
# WHO Life Expectancy — Data Cleaning & Preparation
# =============================================================================

library(tidyverse)
library(mice)
library(readr)

# -----------------------------------------------------------------------------
# 1. Load Raw Data
# -----------------------------------------------------------------------------
cat("Loading raw data...\n")

life_exp_raw <- read_csv("data/raw/life_expectancy_data.csv")

# Standardize column names
names(life_exp_raw) <- gsub(" ", "_", trimws(names(life_exp_raw)))
names(life_exp_raw)[names(life_exp_raw) == "Life_expectancy"] <- "Life_Exp"
names(life_exp_raw)[names(life_exp_raw) == "Adult_Mortality"] <- "Adult_Mort"
names(life_exp_raw)[names(life_exp_raw) == "infant_deaths"]   <- "Infant_Deaths"
names(life_exp_raw)[names(life_exp_raw) == "percentage_expenditure"] <- "Per_Exp"
names(life_exp_raw)[names(life_exp_raw) == "Hepatitis_B"]     <- "HepB"
names(life_exp_raw)[names(life_exp_raw) == "under-five_deaths"] <- "Under5"
names(life_exp_raw)[names(life_exp_raw) == "Total_expenditure"] <- "Total_Exp"
names(life_exp_raw)[names(life_exp_raw) == "HIV/AIDS"]        <- "HIV_AIDS"
names(life_exp_raw)[names(life_exp_raw) == "thinness__1-19_years"] <- "Thin_1_19"
names(life_exp_raw)[names(life_exp_raw) == "thinness_5-9_years"]   <- "Thin_5_9"
names(life_exp_raw)[names(life_exp_raw) == "Income_composition_of_resources"] <- "Inc_Comp_Res"

cat("Dimensions:", nrow(life_exp_raw), "rows x", ncol(life_exp_raw), "cols\n")
cat("Countries:", n_distinct(life_exp_raw$Country), "\n")

# -----------------------------------------------------------------------------
# 2. Handle NAs — Step 1: Add Default Values
# -----------------------------------------------------------------------------
cat("\nStep 1: Adding default values for structural zeros...\n")

# HepB, Polio, Diphtheria: missing often means 0 coverage (not surveyed)
life_exp_raw <- life_exp_raw %>%
  mutate(
    HepB       = ifelse(is.na(HepB), 0, HepB),
    Polio      = ifelse(is.na(Polio), 0, Polio),
    Diphtheria = ifelse(is.na(Diphtheria), 0, Diphtheria)
  )

cat("NA summary after defaults:\n")
print(colSums(is.na(life_exp_raw)))

# -----------------------------------------------------------------------------
# 3. Handle NAs — Step 2: Median Imputation (within country)
# -----------------------------------------------------------------------------
cat("\nStep 2: Within-country median imputation...\n")

numeric_cols <- names(life_exp_raw)[sapply(life_exp_raw, is.numeric)]

life_exp_imputed <- life_exp_raw %>%
  group_by(Country) %>%
  mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  ungroup()

# Remaining NAs: fill with global median
life_exp_imputed <- life_exp_imputed %>%
  mutate(across(all_of(numeric_cols), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Encode Status: Developed = 1, Developing = 0
life_exp_imputed$Status <- ifelse(life_exp_imputed$Status == "Developed", 1, 0)

cat("NAs remaining after imputation:", sum(is.na(life_exp_imputed)), "\n")

# -----------------------------------------------------------------------------
# 4. Time Series → Multivariate: Aggregate by Country
# -----------------------------------------------------------------------------
cat("\nStep 3: Aggregating time series to country-level means...\n")

life_exp <- life_exp_imputed %>%
  group_by(Country, Status) %>%
  summarise(across(all_of(numeric_cols[numeric_cols != "Year"]), mean, na.rm = TRUE),
            .groups = "drop")

cat("Aggregated dataset:", nrow(life_exp), "countries\n")

# -----------------------------------------------------------------------------
# 5. Remove Attributes (pre-correlation)
# Rationale: redundant, highly correlated with kept vars, or poor coverage
# -----------------------------------------------------------------------------
cat("\nStep 4: Removing pre-specified redundant attributes...\n")

drop_pre <- c("Year", "Infant_Deaths", "Per_Exp", "Measles", "Diphtheria",
              "Thin_5_9", "Inc_Comp_Res")

life_exp <- life_exp %>% select(-any_of(drop_pre))

# -----------------------------------------------------------------------------
# 6. Omit Countries with Insufficient Data
# -----------------------------------------------------------------------------
cat("\nStep 5: Omitting countries with excessive missingness...\n")

# Countries that had <3 survey years in original data
sparse_countries <- life_exp_raw %>%
  count(Country) %>%
  filter(n < 3) %>%
  pull(Country)

life_exp <- life_exp %>% filter(!Country %in% sparse_countries)

cat("Countries omitted:", length(sparse_countries), "\n")
cat("Final dataset:", nrow(life_exp), "countries\n")

# -----------------------------------------------------------------------------
# 7. Post-Correlation Removal
# Remove HepB (low variance, weak signal), Under5 (collinear w/ Infant_Deaths),
# Population (near-zero correlation with Life_Exp)
# -----------------------------------------------------------------------------
cat("\nStep 6: Removing post-correlation redundant attributes...\n")

drop_post <- c("HepB", "Under5", "Population")
life_exp  <- life_exp %>% select(-any_of(drop_post))

cat("Final variables:\n")
print(names(life_exp))

# -----------------------------------------------------------------------------
# 8. Save Cleaned Data
# -----------------------------------------------------------------------------
dir.create("data/processed", showWarnings = FALSE)
write_csv(life_exp, "data/processed/life_exp_clean.csv")
cat("\n✓ Saved: data/processed/life_exp_clean.csv\n")
cat("Final dimensions:", nrow(life_exp), "rows x", ncol(life_exp), "cols\n")
