# =============================================================================
# 03_mlm.R
# ESWS Analysis — Multilevel Models (MLM)
#
# Purpose: Fit a series of multilevel (and OLS baseline) models predicting
#          job satisfaction from workforce composition and HR policies.
#          Produces a combined model comparison table and interaction plots.
#
# Model overview:
#   model0  — Exploratory: selected org-level predictors only, no controls
#   model1  — Composite policies, no controls          (MLM)
#   model2  — Composite policies, with controls        (MLM)
#   model3  — Composite policies, no random effects    (OLS baseline)
#   model4  — All individual policies, no controls     (MLM)
#   model5  — All individual policies, with controls   (MLM)
#   model6  — All individual policies, no random effects (OLS baseline)
#
# Input:   esws_clean.rds  (produced by 01_data_preparation.R)
# Output:  table_mlm_comparison.docx
#          Interaction plots displayed in the plotting device
# =============================================================================

source("00_libraries.R")

esws <- readRDS("esws_clean.rds")


# =============================================================================
# 1. VARIABLE LABELS (for model summary table)
# =============================================================================

table_label <- c(
  age_z                           = "Age",
  tenure_z                        = "Tenure",
  org_female_comp_div             = "Gender Diversity (Org)",
  org_senior_comp_div             = "Heterogeneity Senior Workers (Org)",
  org_highskill_comp_div          = "Heterogeneity of Skill Level (Org)",
  org_young_comp_div              = "Heterogeneity Younger Workers (Org)",
  org_wfh_flexpolicy1             = "Policy: Work from Home Flexibility",
  org_wt_flexpolicy1              = "Policy: Working Time Flexibility",
  org_pleave_moth_childpolicy1    = "Policy: Parental Leave (Mothers)",
  org_fewhours_women_childpolicy1 = "Policy: Reduced Hours (Mothers)",
  org_pleave_fath_childpolicy1    = "Policy: Parental Leave (Fathers)",
  org_fewhours_men_childpolicy1   = "Policy: Reduced Hours (Fathers)",
  org_redwh_agepolicy1            = "Policy: Reduced Hours (Senior)",
  org_training_agepolicy1         = "Policy: Training for Senior Employees",
  org_encoach_agepolicy1          = "Policy: Senior Knowledge-Sharing / Coaching",
  org_size_z                      = "Organisation Size",
  dep_female_comp_div             = "Gender Diversity (Dep)",
  dep_senior_comp_div             = "Heterogeneity Senior Workers (Dep)",
  dep_highskill_comp_div          = "Heterogeneity of Skill Level (Dep)",
  dep_young_comp_div              = "Heterogeneity Younger Workers (Dep)",
  dep_size_z                      = "Department Size",
  dep_wfh_flexpolicy              = "Policy: Work from Home Flexibility (Dep)",
  dep_wt_flexpolicy               = "Policy: Working Time Flexibility (Dep)",
  dep_pleave_moth_childpolicy     = "Policy: Parental Leave (Mothers) (Dep)",
  dep_fewhours_women_childpolicy  = "Policy: Reduced Hours (Mothers) (Dep)",
  dep_pleave_fath_childpolicy     = "Policy: Parental Leave (Fathers) (Dep)",
  dep_fewhours_men_childpolicy    = "Policy: Reduced Hours (Fathers) (Dep)",
  dep_redwh_agepolicy             = "Policy: Reduced Hours (Senior) (Dep)",
  dep_training_agepolicy          = "Policy: Training for Senior Employees (Dep)",
  dep_encoach_agepolicy           = "Policy: Senior Knowledge-Sharing / Coaching (Dep)",
  # Composite policy dummies — Gender
  org_any_gender_policyTRUE       = "Any Gender Policy Available (Org)",
  org_any_gender_policyFALSE      = "No Gender Policy Available (Org)",
  dep_any_gender_policyTRUE       = "Any Gender Policy Available (Dep)",
  dep_any_gender_policyFALSE      = "No Gender Policy Available (Dep)",
  # Composite policy dummies — Age
  org_any_age_policyTRUE          = "Any Age Policy Available (Org)",
  org_any_age_policyFALSE         = "No Age Policy Available (Org)",
  dep_any_age_policyTRUE          = "Any Age Policy Available (Dep)",
  dep_any_age_policyFALSE         = "No Age Policy Available (Dep)",
  # Demographics
  gender0                         = "Gender: Female",
  gender1                         = "Gender: Male",
  educationBachelor               = "Education: Bachelor",
  educationBasic                  = "Education: Basic",
  educationPostgraduate           = "Education: Postgraduate",
  educationSecondary              = "Education: Secondary",
  # Sector dummies
  sectorManufacturing             = "Sector: Manufacturing",
  `sectorHealth Care`             = "Sector: Health Care",
  `sectorHigher Education`        = "Sector: Higher Education",
  sectorTransport                 = "Sector: Transport",
  `sectorFinancial Services`      = "Sector: Financial Services",
  sectorTelecommunication         = "Sector: Telecommunication",
  # Country dummies
  countryBulgaria                 = "Country: Bulgaria",
  countryFinland                  = "Country: Finland",
  countryGermany                  = "Country: Germany",
  countryHungary                  = "Country: Hungary",
  countryNetherlands              = "Country: Netherlands",
  countryPortugal                 = "Country: Portugal",
  countrySpain                    = "Country: Spain",
  countrySweden                   = "Country: Sweden",
  countryUK                       = "Country: UK"
)


# =============================================================================
# 2. MODEL FITTING
# =============================================================================

# --- Model 0: Exploratory — selected org predictors, no controls -------------
# Used to explore basic associations before building full models
model0 <- lmer(
  job_satisfaction ~
    org_female_comp +
    org_fewhours_women_childpolicy + org_pleave_fath_childpolicy +
    dep_female_comp +
    (1 | organisationID/departmentID),
  data = esws
)

model0 |> parameters::parameters() |> plot(show_intercept = FALSE)

# --- Model 1: Composite policies, OLS baseline (no random effects) -----------
model1 <- lm(
  job_satisfaction ~
    org_female_comp_div + org_senior_comp_div +
    org_any_gender_policy + org_any_age_policy +
    dep_female_comp_div + dep_senior_comp_div +
    org_female_comp_div:org_any_gender_policy +
    org_senior_comp_div:org_any_age_policy,
  data = esws
)

modelsummary(
  model1,
  stars  = TRUE,
  title  = "Composite policies, no random effects (OLS baseline)",
  output = "kableExtra"
)

# --- Model 2: Composite policies, no controls --------------------------------
model2 <- lmer(
  job_satisfaction ~
    # Organisation-level (Level 3)
    org_female_comp_div + org_senior_comp_div +
    org_any_gender_policy + org_any_age_policy +
    # Department-level (Level 2)
    dep_female_comp_div + dep_senior_comp_div +
    dep_any_gender_policy + dep_any_age_policy +
    # Interactions: composition × policy
    org_female_comp_div:org_any_gender_policy +
    org_senior_comp_div:org_any_age_policy +
    # Random effects
    (1 | organisationID/departmentID),
  data = esws
)


# --- Model 3: Composite policies, with controls ------------------------------
model3 <- lmer(
  job_satisfaction ~
    # Individual-level controls (Level 1)
    age_z + gender + tenure_z + education +
    # Organisation-level (Level 3)
    org_female_comp_div + org_senior_comp_div +
    org_highskill_comp_div + org_young_comp_div +
    org_any_gender_policy + org_any_age_policy +
    org_size_z + sector + country +
    # Department-level (Level 2)
    dep_female_comp_div + dep_senior_comp_div +
    dep_highskill_comp_div + dep_young_comp_div +
    dep_size_z +
    # Interactions
    org_female_comp_div:org_any_gender_policy +
    org_senior_comp_div:org_any_age_policy +
    # Random effects
    (1 | organisationID/departmentID),
  data = esws
)

modelsummary(
  model3,
  stars     = TRUE,
  title     = "Composite policies with controls (MLM)",
  output    = "kableExtra"
)

# --- Model 4: All individual policies, OLS baseline (no random effects) ------
# Org-level policies used here because if a policy exists at the org level,
# it broadly applies to all departments within that organisation.
model4 <- lm(
  job_satisfaction ~
    org_female_comp_div + org_senior_comp_div +
    org_wfh_flexpolicy + org_wt_flexpolicy +
    org_pleave_moth_childpolicy + org_fewhours_women_childpolicy +
    org_pleave_fath_childpolicy + org_fewhours_men_childpolicy +
    org_redwh_agepolicy + org_training_agepolicy + org_encoach_agepolicy +
    dep_female_comp_div + dep_senior_comp_div,
  data = esws
)

modelsummary(
  model4,
  stars  = TRUE,
  title  = "All individual policies, no random effects (OLS baseline)",
  output = "kableExtra"
)


# --- Model 4: All individual policies, no controls ---------------------------
model5 <- lmer(
  job_satisfaction ~
    # Organisation-level (Level 3)
    org_female_comp_div + org_senior_comp_div +
    org_wfh_flexpolicy + org_wt_flexpolicy +
    org_pleave_moth_childpolicy + org_fewhours_women_childpolicy +
    org_pleave_fath_childpolicy + org_fewhours_men_childpolicy +
    org_redwh_agepolicy + org_training_agepolicy + org_encoach_agepolicy +
    # Department-level (Level 2)
    dep_female_comp_div + dep_senior_comp_div +
    dep_wfh_flexpolicy + dep_wt_flexpolicy +
    dep_pleave_moth_childpolicy + dep_fewhours_women_childpolicy +
    dep_pleave_fath_childpolicy + dep_fewhours_men_childpolicy +
    dep_redwh_agepolicy + dep_training_agepolicy + dep_encoach_agepolicy +
    # Random effects
    (1 | organisationID/departmentID),
  data = esws
)

modelsummary(
  model5,
  stars      = TRUE,
  statistic  = "conf.int",
  conf_level = 0.95,
  title      = "All individual policies, no controls (MLM)",
  output     = "kableExtra"
)


# --- Model 6: All individual policies, with controls -------------------------
model6 <- lmer(
  job_satisfaction ~
    # Individual-level controls (Level 1)
    age_z + gender + tenure_z + education +
    # Organisation-level (Level 3)
    org_female_comp_div + org_senior_comp_div +
    org_highskill_comp_div + org_young_comp_div +
    org_wfh_flexpolicy + org_wt_flexpolicy +
    org_pleave_moth_childpolicy + org_fewhours_women_childpolicy +
    org_pleave_fath_childpolicy + org_fewhours_men_childpolicy +
    org_redwh_agepolicy + org_training_agepolicy + org_encoach_agepolicy +
    org_size_z + sector + country +
    # Department-level (Level 2)
    dep_female_comp_div + dep_senior_comp_div +
    dep_highskill_comp_div + dep_young_comp_div +
    dep_size_z +
    # Random effects
    (1 | organisationID/departmentID),
  data = esws
)

modelsummary(
  model6,
  stars  = TRUE,
  title  = "All individual policies, with controls (MLM)",
  output = "kableExtra"
)




# =============================================================================
# 3. COMBINED MODEL COMPARISON TABLE
# =============================================================================

modelsummary(
  list(
    "Model 1"   = model1,
    "Model 2"   = model2,
    "Model 3"   = model3,
    "Model 4"   = model4,
    "Model 5"   = model5,
    "Model 6"   = model6
  ),
  statistic    = "std.error",
  stars        = TRUE,
  fmt          = 2,
  coef_rename  = table_label,
  output       = "table_mlm_comparison.docx"
)

cat("table_mlm_comparison.docx saved.\n")


# =============================================================================
# 4. INTERACTION PLOTS (Model 1)
# =============================================================================

# Extract predicted effects for the two key interactions
eff_gender <- Effect(c("org_female_comp_div", "org_any_gender_policy"), model2)
eff_age    <- Effect(c("org_senior_comp_div", "org_any_age_policy"), model2)

eff_gender_df <- as.data.frame(eff_gender)
eff_age_df    <- as.data.frame(eff_age)

# Plot 1: Gender Diversity × Gender Policy
p_gender <- ggplot(
  eff_gender_df,
  aes(x = org_female_comp_div, y = fit,
      colour = org_any_gender_policy,
      fill   = org_any_gender_policy)
) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  labs(
    x      = "Organisational Gender Diversity",
    y      = "Job Satisfaction",
    colour = "Gender Policy",
    fill   = "Gender Policy",
    title  = "Interaction: Gender Diversity × Gender Policy"
  ) +
  theme_minimal()

print(p_gender)

# Plot 2: Heterogeneity Senior Workers × Age Policy
p_age <- ggplot(
  eff_age_df,
  aes(x = org_senior_comp_div, y = fit,
      colour = org_any_age_policy,
      fill   = org_any_age_policy)
) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  labs(
    x      = "Organisational Heterogeneity: Senior Workers",
    y      = "Job Satisfaction",
    colour = "Age Policy",
    fill   = "Age Policy",
    title  = "Interaction: Heterogeneity Senior Workers × Age Policy"
  ) +
  theme_minimal()

print(p_age)
