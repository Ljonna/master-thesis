# =============================================================================
# 06_gbm_interactions.R
# ESWS Analysis — GBM Interaction Effect Plots
#
# Purpose: Identify and visualise the most important pairwise interaction
#          effects in the fitted XGBoost model using SHAP interaction values.
#          This script picks up directly from 05_gbm_full.R and assumes the
#          following objects are already in the environment:
#
#            - xgb_jobsat  : raw XGBoost fit (from extract_fit_parsnip)
#            - X_shap      : one-hot encoded predictor matrix (numeric)
#
# Approach:
#   1. Compute full SHAP interaction values via treeshap.
#   2. Rank all pairs by mean |SHAP interaction value| — data-driven, no
#      analyst assumptions required.
#   3. Plot the top N pairs as SHAP dependence plots with the interacting
#      variable mapped to colour.
#   4. Produce a summary heatmap and ranked bar chart of the top pairs.
#
# Packages required (in addition to those loaded by 00_libraries.R):
#   treeshap   — install.packages("treeshap")
#   patchwork  — install.packages("patchwork")
# =============================================================================

library(treeshap)
library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
library(forcats)

# =============================================================================
# 1. COMPUTE SHAP INTERACTION VALUES
#
#    treeshap() returns:
#      $shaps        — n x p matrix of main SHAP values
#      $interactions — p x p x n array of pairwise interaction values
#
#    NOTE: can be slow for large datasets; subsample if needed.
# =============================================================================

set.seed(42)

# Subsample rows for speed — increase n_sample if RAM allows
n_sample <- min(nrow(X_shap), 2000)
idx      <- sample(nrow(X_shap), n_sample)
X_sub    <- X_shap[idx, , drop = FALSE]

# Convert XGBoost model to treeshap-compatible unified model
unified <- xgboost.unify(xgb_jobsat, X_sub)

cat("Computing SHAP interaction values (this may take a few minutes)...\n")
shaps <- treeshap(unified, x = X_sub, interactions = TRUE)
cat("Done.\n")


# =============================================================================
# 2. APPLY HUMAN-READABLE VARIABLE LABELS
# =============================================================================

rename_if_mapped <- function(nms, label_map) {
  ifelse(nms %in% names(label_map), label_map[nms], nms)
}

var_labels <- c(
  age_z                             = "Age",
  tenure_z                          = "Tenure",
  org_female_comp_div               = "Gender Diversity (Org)",
  org_senior_comp_div               = "Heterogeneity Senior Workers (Org)",
  org_highskill_comp_div            = "Heterogeneity Skill Level (Org)",
  org_young_comp_div                = "Heterogeneity Younger Workers (Org)",
  org_size_z                        = "Organisation Size",
  dep_female_comp_div               = "Gender Diversity (Dep)",
  dep_senior_comp_div               = "Heterogeneity Senior Workers (Dep)",
  dep_highskill_comp_div            = "Heterogeneity Skill Level (Dep)",
  dep_young_comp_div                = "Heterogeneity Younger Workers (Dep)",
  dep_size_z                        = "Department Size",
  org_wfh_flexpolicy_X0             = "No WFH Policy",
  org_wfh_flexpolicy_X1             = "WFH Policy",
  org_wt_flexpolicy_X0              = "No Working Time Flex Policy",
  org_wt_flexpolicy_X1              = "Working Time Flex Policy",
  org_pleave_moth_childpolicy_X0    = "No Parental Leave (Mothers)",
  org_pleave_moth_childpolicy_X1    = "Parental Leave (Mothers)",
  org_fewhours_women_childpolicy_X0 = "No Reduced Hours (Mothers)",
  org_fewhours_women_childpolicy_X1 = "Reduced Hours (Mothers)",
  org_pleave_fath_childpolicy_X0    = "No Parental Leave (Fathers)",
  org_pleave_fath_childpolicy_X1    = "Parental Leave (Fathers)",
  org_fewhours_men_childpolicy_X0   = "No Reduced Hours (Fathers)",
  org_fewhours_men_childpolicy_X1   = "Reduced Hours (Fathers)",
  org_redwh_agepolicy_X0            = "No Reduced Hours (Senior)",
  org_redwh_agepolicy_X1            = "Reduced Hours (Senior)",
  org_training_agepolicy_X0         = "No Training (Senior)",
  org_training_agepolicy_X1         = "Training (Senior)",
  org_encoach_agepolicy_X0          = "No Coaching (Senior)",
  org_encoach_agepolicy_X1          = "Coaching (Senior)",
  gender_X0                         = "Gender: Female",
  gender_X1                         = "Gender: Male",
  education_Bachelor                = "Education: Bachelor",
  education_Basic                   = "Education: Basic",
  education_Postgraduate            = "Education: Postgraduate",
  education_Secondary               = "Education: Secondary",
  sector_X1                         = "Sector: Manufacturing",
  sector_X2                         = "Sector: Health Care",
  sector_X3                         = "Sector: Higher Education",
  sector_X4                         = "Sector: Transport",
  sector_X5                         = "Sector: Financial Services",
  sector_X6                         = "Sector: Telecommunication",
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

# Label main SHAP matrix columns
nms <- rename_if_mapped(colnames(X_sub), var_labels)
colnames(shaps$shaps) <- nms

# $interactions is a p x p x n array — label dimensions 1 and 2
int_raw <- shaps$interactions
dimnames(int_raw)[[1]] <- nms
dimnames(int_raw)[[2]] <- nms


# =============================================================================
# 3. RANK PAIRWISE INTERACTIONS BY MEAN |SHAP INTERACTION VALUE|
# =============================================================================

# Average absolute interaction values across observations (3rd dimension)
mean_abs_int <- apply(abs(int_raw), c(1, 2), mean)

# One row per unique pair, excluding self-interactions
pairs_df <- as.data.frame(as.table(mean_abs_int)) |>
  rename(var1 = Var1, var2 = Var2, mean_abs_shap_int = Freq) |>
  mutate(across(c(var1, var2), as.character)) |>
  filter(var1 < var2) |>
  arrange(desc(mean_abs_shap_int))

cat("\nTop 20 pairwise interactions by mean |SHAP interaction value|:\n")
print(head(pairs_df, 20))

# Number of top interactions to plot individually as dependence plots
n_top <- 6


# =============================================================================
# Define dummy groups — pairs within the same group are coding artefacts
# and should be excluded from the interaction ranking
# =============================================================================

dummy_groups <- list(
  country   = grep("^Country:",   unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  sector    = grep("^Sector:",    unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  education = grep("^Education:", unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  gender    = grep("^Gender:",    unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  # Policy pairs — same underlying question, different levels
  wfh       = grep("WFH Policy$",           unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  wt        = grep("Working Time Flex",      unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  pleave_m  = grep("Parental Leave \\(Moth", unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  pleave_f  = grep("Parental Leave \\(Fath", unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  rhours_w  = grep("Reduced Hours \\(Moth",  unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  rhours_m  = grep("Reduced Hours \\(Fath",  unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  rhours_s  = grep("Reduced Hours \\(Seni",  unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  training  = grep("Training \\(Senior\\)",  unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE),
  coaching  = grep("Coaching \\(Senior\\)",  unique(c(pairs_df$var1, pairs_df$var2)), value = TRUE)
)

# Build a set of within-group pairs to exclude
within_group_pairs <- do.call(rbind, lapply(dummy_groups, function(grp) {
  if (length(grp) < 2) return(NULL)
  pairs <- combn(sort(grp), 2, simplify = FALSE)
  data.frame(
    var1 = sapply(pairs, `[`, 1),
    var2 = sapply(pairs, `[`, 2),
    stringsAsFactors = FALSE
  )
}))

# Filter them out
pairs_df <- pairs_df |>
  anti_join(within_group_pairs, by = c("var1", "var2"))

# =============================================================================
# 4. HEATMAP OF TOP INTERACTIONS
# =============================================================================

# Variables most involved in strong interactions
top_vars <- pairs_df |>
  pivot_longer(c(var1, var2), values_to = "var") |>
  group_by(var) |>
  summarise(total_int = sum(mean_abs_shap_int), .groups = "drop") |>
  slice_max(total_int, n = 15) |>
  pull(var)

# Build symmetric data frame for the heatmap
heatmap_df <- pairs_df |>
  filter(var1 %in% top_vars, var2 %in% top_vars) |>
  rename(value = mean_abs_shap_int)

heatmap_df <- bind_rows(
  heatmap_df,
  heatmap_df |> rename(var1 = var2, var2 = var1)
)

p_heatmap <- ggplot(heatmap_df, aes(
    x    = fct_reorder(var1, value, .fun = sum),
    y    = fct_reorder(var2, value, .fun = sum),
    fill = value
  )) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_viridis_c(
    name      = "Mean |SHAP\ninteraction|",
    option    = "magma",
    direction = -1
  ) +
  labs(
    title    = "Pairwise SHAP interaction strengths",
    subtitle = "Top 15 variables by total interaction involvement",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y     = element_text(size = 9),
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10, colour = "grey40"),
    legend.position = "right",
    panel.grid      = element_blank()
  )

print(p_heatmap)


# =============================================================================
# 5. RANKED BAR CHART OF TOP INTERACTION PAIRS
# =============================================================================

top_n_pairs <- pairs_df |>
  slice_head(n = 15) |>
  mutate(pair_label = paste0(var1, "  ×\n", var2))

p_bar <- ggplot(
    top_n_pairs,
    aes(
      x = mean_abs_shap_int,
      y = fct_reorder(pair_label, mean_abs_shap_int)
    )
  ) +
  geom_col(fill = "#404080", width = 0.7) +
  labs(
    title    = "Top 15 pairwise interactions",
    subtitle = "Ranked by mean |SHAP interaction value|",
    x        = "Mean |SHAP interaction value|",
    y        = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(size = 10, colour = "grey40"),
    axis.text.y        = element_text(size = 8),
    panel.grid.major.y = element_blank()
  )

print(p_bar)
