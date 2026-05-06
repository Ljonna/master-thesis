# =============================================================================
# 05_gbm_full.R
# ESWS Analysis — Gradient Boosting Machine (All Individual Policies)
#
# Purpose: Fit a full XGBoost model that includes all individual
#          policy variables (rather than composite). Uses a proper
#          tidymodels recipe with one-hot encoding of categorical variables,
#          grouped cross-validation, hyperparameter tuning, and SHAP plots
#          with human-readable variable labels.
#
# Input:   esws_clean.rds  (produced by 01_data_preparation.R)
# Output:  SHAP plots displayed in the plotting device
#          xgb_jobsat — fitted XGBoost model object (in environment)
# =============================================================================

source("00_libraries.R")

esws <- readRDS("esws_clean.rds")


# =============================================================================
# 1. PREPARE DATA
# =============================================================================

# Variables to include in this model
vars <- c(
  "job_satisfaction",
  # Individual-level
  "age_z", "gender", "tenure_z", "education",
  # Organisation-level
  "org_female_comp_div", "org_senior_comp_div",
  "org_highskill_comp_div", "org_young_comp_div",
  "org_wfh_flexpolicy", "org_wt_flexpolicy",
  "org_pleave_moth_childpolicy", "org_fewhours_women_childpolicy",
  "org_pleave_fath_childpolicy", "org_fewhours_men_childpolicy",
  "org_redwh_agepolicy", "org_training_agepolicy", "org_encoach_agepolicy",
  "org_size_z", "sector", "country",
  # Department-level
  "dep_female_comp_div", "dep_senior_comp_div",
  "dep_highskill_comp_div", "dep_young_comp_div",
  "dep_size_z",
  # Organisation ID — needed for grouped folds, removed before modelling
  "organisationID"
)

esws_model2 <- esws |>
  select(all_of(vars)) |>
  drop_na() |>
  mutate(
    gender    = as.factor(gender),
    education = as.factor(education),
    sector    = as.factor(sector),
    country   = as.factor(country)
  )


# =============================================================================
# 2. GROUPED CROSS-VALIDATION
# =============================================================================

set.seed(123)

folds <- group_vfold_cv(
  esws_model2,
  group = "organisationID",
  v     = 5
)

print(folds)


# =============================================================================
# 3. PREPROCESSING RECIPE
#
#    organisationID is removed before modelling (it was only needed for folds).
#    Categorical predictors are one-hot encoded. Zero-variance columns
#    (e.g. a dummy level that never appears) are dropped automatically.
# =============================================================================

xgb_recipe <- recipe(job_satisfaction ~ ., data = esws_model2) |>
  step_rm(organisationID) |>
  step_dummy(all_nominal_predictors(), one_hot = TRUE) |>
  step_zv(all_predictors())


# =============================================================================
# 4. MODEL SPECIFICATION
# =============================================================================

xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  loss_reduction = tune(),
  sample_size    = tune(),
  mtry           = tune(),
  min_n          = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")


# =============================================================================
# 5. TUNING GRID
# =============================================================================

xgb_grid <- grid_regular(
  trees(range          = c(100, 500)),
  tree_depth(range     = c(3, 6)),
  learn_rate(range     = c(-2, -1)),
  loss_reduction(range = c(-1, 0)),
  sample_prop(range    = c(0.7, 0.9)),
  mtry(range           = c(5, 20)),
  min_n(range          = c(5, 20)),
  levels = 3
)


# =============================================================================
# 6. WORKFLOW AND TUNING
# =============================================================================

wf_jobsat <- workflow() |>
  add_recipe(xgb_recipe) |>
  add_model(xgb_spec)

tune_jobsat <- tune_grid(
  wf_jobsat,
  resamples = folds,
  grid      = xgb_grid,
  metrics   = metric_set(rmse, rsq),
  control   = control_grid(verbose = TRUE)
)


# =============================================================================
# 7. INSPECT RESULTS
# =============================================================================

collect_metrics(tune_jobsat)
autoplot(tune_jobsat)

cat("\nBest hyperparameters (RMSE):\n")
print(select_best(tune_jobsat, metric = "rmse"))


# =============================================================================
# 8. FINALISE MODEL
# =============================================================================

best_jobsat  <- select_best(tune_jobsat, metric = "rmse")

final_jobsat <- finalize_workflow(wf_jobsat, best_jobsat) |>
  fit(data = esws_model2)

# Extract the raw XGBoost object for SHAP analysis
xgb_jobsat <- extract_fit_parsnip(final_jobsat)$fit


# =============================================================================
# 9. SHAP ANALYSIS
# =============================================================================

# Bake training data through the recipe to get the one-hot encoded matrix
prepped_data <- extract_recipe(final_jobsat) |>
  bake(new_data = esws_model2)

X_shap <- prepped_data |>
  select(-job_satisfaction) |>
  as.matrix()

shap_jobsat2 <- shapviz(xgb_jobsat, X_pred = X_shap)

# Diagnostic: verify matrix dimensions and column alignment before renaming
cat("SHAP S matrix dimensions:", dim(shap_jobsat2$S), "\n")
cat("SHAP X matrix dimensions:", dim(shap_jobsat2$X), "\n")
cat("Column counts match:", ncol(shap_jobsat2$S) == ncol(shap_jobsat2$X), "\n")
cat("Any NA column names:", sum(is.na(colnames(shap_jobsat2$S))), "\n")


# --- Human-readable variable labels ------------------------------------------
var_labels <- c(
  age_z                          = "Age",
  tenure_z                       = "Tenure",
  org_female_comp_div            = "Gender Diversity (Org)",
  org_senior_comp_div            = "Heterogeneity Senior Workers (Org)",
  org_highskill_comp_div         = "Heterogeneity Skill Level (Org)",
  org_young_comp_div             = "Heterogeneity Younger Workers (Org)",
  org_size_z                     = "Organisation Size",
  dep_female_comp_div            = "Gender Diversity (Dep)",
  dep_senior_comp_div            = "Heterogeneity Senior Workers (Dep)",
  dep_highskill_comp_div         = "Heterogeneity Skill Level (Dep)",
  dep_young_comp_div             = "Heterogeneity Younger Workers (Dep)",
  dep_size_z                     = "Department Size",
  # Policy dummies (one-hot encoded as _X0 / _X1)
  org_wfh_flexpolicy_X0             = "No Policy: Work from Home Flexibility",
  org_wfh_flexpolicy_X1             = "Policy: Work from Home Flexibility",
  org_wt_flexpolicy_X0              = "No Policy: Working Time Flexibility",
  org_wt_flexpolicy_X1              = "Policy: Working Time Flexibility",
  org_pleave_moth_childpolicy_X0    = "No Policy: Parental Leave (Mothers)",
  org_pleave_moth_childpolicy_X1    = "Policy: Parental Leave (Mothers)",
  org_fewhours_women_childpolicy_X0 = "No Policy: Reduced Hours (Mothers)",
  org_fewhours_women_childpolicy_X1 = "Policy: Reduced Hours (Mothers)",
  org_pleave_fath_childpolicy_X0    = "No Policy: Parental Leave (Fathers)",
  org_pleave_fath_childpolicy_X1    = "Policy: Parental Leave (Fathers)",
  org_fewhours_men_childpolicy_X0   = "No Policy: Reduced Hours (Fathers)",
  org_fewhours_men_childpolicy_X1   = "Policy: Reduced Hours (Fathers)",
  org_redwh_agepolicy_X0            = "No Policy: Reduced Hours (Senior)",
  org_redwh_agepolicy_X1            = "Policy: Reduced Hours (Senior)",
  org_training_agepolicy_X0         = "No Policy: Training for Senior Employees",
  org_training_agepolicy_X1         = "Policy: Training for Senior Employees",
  org_encoach_agepolicy_X0          = "No Policy: Senior Knowledge-Sharing / Coaching",
  org_encoach_agepolicy_X1          = "Policy: Senior Knowledge-Sharing / Coaching",
  # Gender dummies
  gender_X0                         = "Gender: Female",
  gender_X1                         = "Gender: Male",
  # Education dummies
  education_Bachelor                = "Education: Bachelor",
  education_Basic                   = "Education: Basic",
  education_Postgraduate            = "Education: Postgraduate",
  education_Secondary               = "Education: Secondary",
  # Sector dummies
  sector_X1                         = "Sector: Manufacturing",
  sector_X2                         = "Sector: Health Care",
  sector_X3                         = "Sector: Higher Education",
  sector_X4                         = "Sector: Transport",
  sector_X5                         = "Sector: Financial Services",
  sector_X6                         = "Sector: Telecommunication",
  # Country dummies
  country_Bulgaria                  = "Country: Bulgaria",
  country_Finland                   = "Country: Finland",
  country_Germany                   = "Country: Germany",
  country_Hungary                   = "Country: Hungary",
  country_Netherlands               = "Country: Netherlands",
  country_Portugal                  = "Country: Portugal",
  country_Spain                     = "Country: Spain",
  country_Sweden                    = "Country: Sweden",
  country_UK                        = "Country: UK"
)

# Apply labels (only rename columns that have a match in var_labels)
rename_if_mapped <- function(nms, label_map) {
  ifelse(nms %in% names(label_map), label_map[nms], nms)
}

colnames(shap_jobsat2$S) <- rename_if_mapped(colnames(shap_jobsat2$S), var_labels)
colnames(shap_jobsat2$X) <- rename_if_mapped(colnames(shap_jobsat2$X), var_labels)


# --- SHAP plots --------------------------------------------------------------

# Overall importance — bar chart
sv_importance(shap_jobsat2, kind = "bar", max_display = 20)

# Overall importance and direction — beeswarm
sv_importance(shap_jobsat2, kind = "beeswarm", max_display = 20)

# Dependence plot: gender diversity × gender policy interaction
sv_dependence(
  shap_jobsat2,
  v         = "Gender Diversity (Org)",
  color_var = "Policy: Reduced Hours (Fathers)"
)

# Dependence plot: senior heterogeneity × age policy interaction
sv_dependence(
  shap_jobsat2,
  v         = "Heterogeneity Senior Workers (Org)",
  color_var = "Policy: Reduced Hours (Senior)"
)
