# =============================================================================
# 00_libraries.R
# ESWS Analysis — Package Loading
# 
# Purpose: Load all packages required across the ESWS analysis pipeline.
#          Source this file at the top of each script.
# =============================================================================

# --- Data import & wrangling -------------------------------------------------
library(tidyverse)
library(haven)
library(purrr)
library(tibble)

# --- Missing data imputation -------------------------------------------------
library(mice)

# --- Descriptive tables ------------------------------------------------------
library(gtsummary)
library(gt)

# --- Multilevel modelling (MLM) ----------------------------------------------
library(lme4)
library(lmerTest)
library(modelsummary)
library(easystats)
library(car)

# --- Plotting ----------------------------------------------------------------
library(effects)
library(ggplot2)

# --- Gradient Boosting Machine (GBM / XGBoost) --------------------------------
library(tidymodels)
library(rsample)     # group_vfold_cv, vfold_cv
library(parsnip)     # boost_tree, set_engine, set_mode
library(workflows)   # workflow, add_model, add_formula / add_recipe
library(tune)        # tune, tune_grid, select_best, collect_metrics
library(yardstick)   # metric_set, rmse, rsq
library(dials)       # grid_regular, trees, learn_rate, etc.
library(xgboost)

# --- SHAP values -------------------------------------------------------------
library(shapviz)
library(SHAPforxgboost)
