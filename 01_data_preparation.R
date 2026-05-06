# =============================================================================
# 01_data_preparation.R
# ESWS Analysis — Data Preparation
#
# Purpose: Load raw data, select and rename variables, clean country-specific
#          education variables, recode, impute missing values with MICE, and
#          produce the analysis-ready 'esws' data frame.
#
# Output:  esws  — clean, imputed, analysis-ready data frame saved as
#                  "esws_clean.rds" for downstream scripts.
# =============================================================================
setwd("~/Documents/Thesis/ESWS data/cleaned_files")
source("00_libraries.R")


# =============================================================================
# 1. LOAD RAW DATA
# =============================================================================

esws_eq_raw <- read_dta("ESWS EQ w1.dta")


# =============================================================================
# 2. SELECT AND RENAME VARIABLES
# =============================================================================

esws_eq <- esws_eq_raw |>
  select(
    country          = country,
    sector           = sector,
    respondentID     = username,
    departmentID     = department,
    organisationID   = organisationID,
    tenure           = Q1_1,
    gender           = Q11,
    age              = Q12,
    # Country-specific education variables (harmonised below)
    uk_education     = Q14_UK,
    de_education     = Q14_DE,
    fi_education     = Q14_FI,
    se_education     = Q14_SE,
    nl_education     = Q14_NL,
    pt_education     = Q14_PT,
    es_education     = Q14_ES,
    hu_education     = Q14_HU,
    bg_education     = Q14_BG,
    # Outcome variables
    job_satisfaction    = Q73,
    # Department-level variables
    dep_size                       = MQ1,
    dep_female_comp                = MQ2_1,
    dep_young_comp                 = MQ2_2,
    dep_senior_comp                = MQ2_3,
    dep_highskill_comp             = MQ2_4,
    dep_wfh_flexpolicy             = MQ9_1,
    dep_wt_flexpolicy              = MQ9_3,
    dep_pleave_moth_childpolicy    = MQ16_2,
    dep_fewhours_women_childpolicy = MQ16_4,
    dep_pleave_fath_childpolicy    = MQ19_2,
    dep_fewhours_men_childpolicy   = MQ19_4,
    dep_redwh_agepolicy            = MQ28_3,
    dep_training_agepolicy         = MQ28_5,
    dep_encoach_agepolicy          = MQ28_6,
    # Organisation-level variables
    org_size                       = OQ4,
    org_female_comp                = OQ5_1,
    org_young_comp                 = OQ5_2,
    org_senior_comp                = OQ5_3,
    org_highskill_comp             = OQ5_4,
    org_wfh_flexpolicy             = OQ20_1,
    org_wt_flexpolicy              = OQ20_3,
    org_pleave_moth_childpolicy    = OQ29_2,
    org_fewhours_women_childpolicy = OQ29_4,
    org_pleave_fath_childpolicy    = OQ32_2,
    org_fewhours_men_childpolicy   = OQ32_4,
    org_redwh_agepolicy            = OQ42_3,
    org_training_agepolicy         = OQ42_5,
    org_encoach_agepolicy          = OQ42_6
  )


# =============================================================================
# 3. HARMONISE COUNTRY-SPECIFIC EDUCATION VARIABLES
#
#    Each country used a different survey question for education level.
#    This helper function selects the correct column, renames it to 'education',
#    and applies a shared recoding scheme to produce four ordinal categories.
# =============================================================================

recode_education <- function(edu_value) {
  case_when(
    edu_value == 99999 ~ NA_character_,
    edu_value <= 4     ~ "Basic",
    edu_value == 5     ~ "Secondary",
    edu_value == 6     ~ "Bachelor",
    edu_value >= 7     ~ "Postgraduate",
    TRUE               ~ NA_character_
  )
}

# Country lookup: country label → country-specific education column name
country_edu_map <- list(
  "UK"          = "uk_education",
  "Germany"     = "de_education",
  "Finland"     = "fi_education",
  "Sweden"      = "se_education",
  "Netherlands" = "nl_education",
  "Portugal"    = "pt_education",
  "Spain"       = "es_education",
  "Hungary"     = "hu_education",
  "Bulgaria"    = "bg_education"
)

# Shared set of variables to keep after education harmonisation
core_vars <- c(
  "country", "sector", "respondentID", "departmentID", "organisationID",
  "tenure", "gender", "age", "education",
  "job_satisfaction", "dep_size", "dep_female_comp", "dep_young_comp",
  "dep_senior_comp", "dep_highskill_comp", "dep_wfh_flexpolicy", 
  "dep_wt_flexpolicy", "dep_pleave_moth_childpolicy", "dep_fewhours_women_childpolicy",
  "dep_pleave_fath_childpolicy", "dep_fewhours_men_childpolicy",
  "dep_redwh_agepolicy", "dep_training_agepolicy", "dep_encoach_agepolicy",
  "org_size", "org_female_comp", "org_young_comp", "org_senior_comp",
  "org_highskill_comp",
  "org_wfh_flexpolicy", "org_wt_flexpolicy",
  "org_pleave_moth_childpolicy", "org_fewhours_women_childpolicy",
  "org_pleave_fath_childpolicy", "org_fewhours_men_childpolicy",
  "org_redwh_agepolicy", "org_training_agepolicy", "org_encoach_agepolicy"
)

clean_country <- function(data, country_label, edu_col) {
  data |>
    filter(country == country_label) |>
    rename(education = all_of(edu_col)) |>
    mutate(education = recode_education(education)) |>
    select(all_of(core_vars))
}

# Apply cleaning function to each country and bind into one data frame
esws_eq_final <- map2_dfr(
  names(country_edu_map),
  country_edu_map,
  ~clean_country(esws_eq, .x, .y)
)


# =============================================================================
# 4. INITIAL RECODING
# =============================================================================

# --- Tenure ------------------------------------------------------------------
# Code 99996 means "less than 1 year" — recode to 0 for interpretation
esws_eq_final <- esws_eq_final |>
  mutate(tenure = if_else(tenure == 99996, 0, tenure))

# --- Education factor --------------------------------------------------------
esws_eq_final$education <- factor(
  esws_eq_final$education,
  levels = c("Basic", "Secondary", "Bachelor", "Postgraduate")
)


# =============================================================================
# 5. REMOVE STATA LABELS AND REPLACE SENTINEL MISSING CODES
# =============================================================================

# Convert labelled numerics to plain numerics (removes haven labels)
esws_clean <- esws_eq_final |>
  mutate(across(where(is.labelled), ~zap_missing(.x))) |>
  mutate(across(where(is.labelled), ~zap_labels(.x)))

# Diagnostic: check for sentinel values and existing NAs
check_data <- function(df) {
  tibble(
    column       = names(df),
    data_type    = map_chr(df, ~class(.x)[1]),
    na_count     = map_int(df, ~sum(is.na(.x))),
    count_99996  = map_int(df, ~sum(.x == 99996, na.rm = TRUE)),
    count_99997  = map_int(df, ~sum(.x == 99997, na.rm = TRUE)),
    count_99999  = map_int(df, ~sum(.x == 99999, na.rm = TRUE))
  )
}

cat("--- Data summary before sentinel replacement ---\n")
print(check_data(esws_clean))

# Replace all sentinel codes with NA
esws_no_na <- esws_clean |>
  mutate(across(everything(), ~replace(.x, .x %in% c(99996, 99997, 99999), NA)))

cat("\nTotal NAs after sentinel replacement:", sum(is.na(esws_no_na)), "\n")

# Ensure policy variables are stored as factors (required by MICE logreg)
esws_no_na <- esws_no_na |>
  mutate(across(ends_with("policy"), factor))


# =============================================================================
# 6. MULTIPLE IMPUTATION (MICE)
#
#    Only age-related policy variables at both department and organisation
#    level are imputed (logreg). All other policy variables and gender are
#    left as-is (method = "") because their missingness is structural
#    (not all policies exist in all countries/sectors).
# =============================================================================

# Initialise to get default method and predictor matrix
ini <- mice(esws_no_na, maxit = 0)

# Variables NOT to impute (structural missingness)
no_impute_vars <- c(
  "gender",
  "dep_wfh_flexpolicy", "dep_wt_flexpolicy",
  "dep_pleave_moth_childpolicy", "dep_pleave_fath_childpolicy",
  "dep_fewhours_women_childpolicy", "dep_fewhours_men_childpolicy",
  "org_wfh_flexpolicy", "org_wt_flexpolicy",
  "org_pleave_moth_childpolicy", "org_pleave_fath_childpolicy",
  "org_fewhours_women_childpolicy", "org_fewhours_men_childpolicy"
)

ini$method[no_impute_vars] <- ""

# Variables to impute with logistic regression
logreg_vars <- c(
  "dep_redwh_agepolicy", "dep_training_agepolicy", "dep_encoach_agepolicy",
  "org_redwh_agepolicy", "org_training_agepolicy", "org_encoach_agepolicy"
)

ini$method[logreg_vars] <- "logreg"

# Run imputation
imputed <- mice(
  esws_no_na,
  method          = ini$method,
  predictorMatrix = ini$predictorMatrix,
  m               = 5,
  maxit           = 50,
  seed            = 123
)

# Extract first imputed dataset
esws_complete <- complete(imputed, 1)
cat("\nTotal NAs after imputation:", sum(is.na(esws_complete)), "\n")

# Remove remaining rows with NA (variables not imputed above)
esws <- esws_complete |> na.omit()

cat("--- Final data summary ---\n")
print(check_data(esws))


# =============================================================================
# 7. FINAL VARIABLE RECODING AND FEATURE ENGINEERING
# =============================================================================

# --- Sector ------------------------------------------------------------------
esws$sector <- factor(
  esws$sector,
  levels = c(1, 2, 3, 4, 5, 6),
  labels = c("Manufacturing", "Health Care", "Higher Education",
             "Transport", "Financial Services", "Telecommunication")
)

# --- Education reference level -----------------------------------------------
esws$education <- relevel(factor(esws$education), ref = "Basic")

# --- Binary policy and gender variables (2 = Yes → 1; 1 = No → 0) -----------
# Note: original coding has 1 = No and 2 = Yes, which is counterintuitive.
# Recoded so that 1 = policy exists / male, 0 = policy absent / female.
binary_vars <- c(
  "gender",
  "dep_wfh_flexpolicy", "dep_wt_flexpolicy",
  "dep_pleave_moth_childpolicy", "dep_pleave_fath_childpolicy",
  "dep_fewhours_women_childpolicy", "dep_fewhours_men_childpolicy",
  "dep_redwh_agepolicy", "dep_training_agepolicy", "dep_encoach_agepolicy",
  "org_wfh_flexpolicy", "org_wt_flexpolicy",
  "org_pleave_moth_childpolicy", "org_pleave_fath_childpolicy",
  "org_fewhours_women_childpolicy", "org_fewhours_men_childpolicy",
  "org_redwh_agepolicy", "org_training_agepolicy", "org_encoach_agepolicy"
)

esws <- esws |>
  mutate(across(all_of(binary_vars), ~ifelse(.x == 2, 1, 0)))

# Gender as factor with effect coding (sum-to-zero contrast)
# This centres the gender effect around the grand mean rather than a reference
esws$gender <- factor(esws$gender)
contrasts(esws$gender) <- contr.sum(2)

# Organisation-level binary vars as factors (needed for modelling)
esws <- esws |>
  mutate(across(starts_with("org_") & where(~all(. %in% c(0, 1, NA))), as.factor))

# --- Diversity (folded) composition variables --------------------------------
# Composition variables are on a 1–9 scale where 1 = no women/young/senior
# and 9 = all women/young/senior. Folding around the midpoint (5) means the
# highest score represents maximum heterogeneity/diversity.
esws <- esws |>
  mutate(
    dep_female_comp_div    = 5 - abs(dep_female_comp - 5),
    dep_young_comp_div     = 5 - abs(dep_young_comp - 5),
    dep_senior_comp_div    = 5 - abs(dep_senior_comp - 5),
    dep_highskill_comp_div = 5 - abs(dep_highskill_comp - 5),
    org_female_comp_div    = 5 - abs(org_female_comp - 5),
    org_young_comp_div     = 5 - abs(org_young_comp - 5),
    org_senior_comp_div    = 5 - abs(org_senior_comp - 5),
    org_highskill_comp_div = 5 - abs(org_highskill_comp - 5)
  )

# --- Composite policy variables ----------------------------------------------
# Whether any policy of a given type exists at department or organisation level
esws <- esws |>
  mutate(
    org_any_gender_policy = as.factor(if_any(c(
      org_wfh_flexpolicy, org_wt_flexpolicy,
      org_pleave_moth_childpolicy, org_fewhours_women_childpolicy,
      org_pleave_fath_childpolicy, org_fewhours_men_childpolicy
    ), ~ as.numeric(as.character(.x)) == 1)),
    
    dep_any_gender_policy = as.factor(if_any(c(
      dep_wfh_flexpolicy, dep_wt_flexpolicy,
      dep_pleave_moth_childpolicy, dep_fewhours_women_childpolicy,
      dep_pleave_fath_childpolicy, dep_fewhours_men_childpolicy
    ), ~ as.numeric(as.character(.x)) == 1)),
    
    org_any_age_policy = as.factor(if_any(c(
      org_redwh_agepolicy, org_training_agepolicy, org_encoach_agepolicy
    ), ~ as.numeric(as.character(.x)) == 1)),
    
    dep_any_age_policy = as.factor(if_any(c(
      dep_redwh_agepolicy, dep_training_agepolicy, dep_encoach_agepolicy
    ), ~ as.numeric(as.character(.x)) == 1))
  )

# --- Z-score standardisation of continuous variables ------------------------
# Standardising continuous variables at each level for comparability in MLM
esws <- esws |>
  mutate(
    age_z      = as.numeric(scale(age)),
    tenure_z   = as.numeric(scale(tenure)),
    org_size_z = as.numeric(scale(org_size)),
    dep_size_z = as.numeric(scale(dep_size))
  )


# =============================================================================
# 8. SAVE CLEAN DATA
# =============================================================================

saveRDS(esws, "esws_clean.rds")
cat("\nesws_clean.rds saved. Final dimensions:", nrow(esws), "rows ×", ncol(esws), "columns\n")
