# =============================================================================
# 02_descriptives.R
# ESWS Analysis — Descriptive Statistics
#
# Purpose: Produce Table 1 (descriptive statistics) and check variable
#          distributions. Exports table as a Word document.
#
# Input:   esws_clean.rds  (produced by 01_data_preparation.R)
# Output:  table1_descriptives.docx
# =============================================================================

source("00_libraries.R")

esws <- readRDS("esws_clean.rds")


# =============================================================================
# 1. VARIABLE DISTRIBUTION CHECKS
# =============================================================================

# Quick check of policy distribution by country (example: UK)
esws |>
  filter(country == "UK") |>
  count(org_pleave_moth_childpolicy) |>
  mutate(proportion = n / sum(n))

# Visual check of a composition variable distribution
barplot(
  table(esws$dep_female_comp),
  main = "Distribution of Department Female Composition",
  xlab = "Category (1–9)",
  ylab = "Count",
  col  = "steelblue"
)

# Multicollinearity check for key composition variables
modell <- lm(job_satisfaction ~ org_female_comp + dep_female_comp, data = esws)
vif(modell)


# =============================================================================
# 2. TABLE 1 — DESCRIPTIVE STATISTICS
# =============================================================================

# --- 2a. Define variable groups ----------------------------------------------

individual_vars <- c("job_satisfaction", "gender", "age", "tenure", "education")

department_vars <- c(
  "dep_size", "dep_female_comp_div", "dep_young_comp_div",
  "dep_senior_comp_div", "dep_highskill_comp_div"
)

org_vars <- c(
  "org_size", "org_female_comp_div", "org_young_comp_div",
  "org_senior_comp_div", "org_highskill_comp_div"
)

# Gender/family policies (moderators interacting with female composition)
policy_gender <- c(
  "org_wfh_flexpolicy", "org_wt_flexpolicy",
  "org_pleave_moth_childpolicy", "org_fewhours_women_childpolicy",
  "org_pleave_fath_childpolicy", "org_fewhours_men_childpolicy"
)

# Age-related policies (moderators interacting with senior/young composition)
policy_age <- c(
  "org_redwh_agepolicy", "org_training_agepolicy", "org_encoach_agepolicy"
)

all_vars <- c(individual_vars, department_vars, org_vars, policy_gender, policy_age)

# --- 2b. Build and export table ----------------------------------------------

esws |>
  select(all_of(all_vars)) |>
  tbl_summary(
    statistic = list(
      all_continuous()  ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_continuous()  ~ 2,
      all_categorical() ~ c(0, 1)
    ),
    missing = "no",
    label = list(
      job_satisfaction               ~ "Job Satisfaction",
      gender                         ~ "Gender (Female = 0)",
      age                            ~ "Age (years)",
      tenure                         ~ "Tenure (years)",
      education                      ~ "Education Level",
      dep_size                       ~ "Department Size",
      dep_female_comp_div            ~ "Department Gender Diversity (1–5)",
      dep_young_comp_div             ~ "Department Heterogeneity of Younger Employees (1–5)",
      dep_senior_comp_div            ~ "Department Heterogeneity of Senior Employees (1–5)",
      dep_highskill_comp_div         ~ "Department Heterogeneity of High-Skill Employees (1–5)",
      org_size                       ~ "Organisation Size",
      org_female_comp_div            ~ "Organisation Gender Diversity (1–5)",
      org_young_comp_div             ~ "Organisation Heterogeneity of Younger Employees (1–5)",
      org_senior_comp_div            ~ "Organisation Heterogeneity of Senior Employees (1–5)",
      org_highskill_comp_div         ~ "Organisation Heterogeneity of High-Skill Employees (1–5)",
      org_wfh_flexpolicy             ~ "Work-from-Home Flexibility Policy",
      org_wt_flexpolicy              ~ "Working-Time Flexibility Policy",
      org_pleave_moth_childpolicy    ~ "Parental Leave Policy (Mothers)",
      org_fewhours_women_childpolicy ~ "Reduced Hours for Women (Child) Policy",
      org_pleave_fath_childpolicy    ~ "Parental Leave Policy (Fathers)",
      org_fewhours_men_childpolicy   ~ "Reduced Hours for Men (Child) Policy",
      org_redwh_agepolicy            ~ "Reduced Working Hours (Age) Policy",
      org_training_agepolicy         ~ "Training for Older Workers Policy",
      org_encoach_agepolicy          ~ "Senior Coaching (Age) Policy"
    )
  ) |>
  modify_table_body(
    ~.x |>
      mutate(groupname_col = case_when(
        variable %in% individual_vars ~ "Individual-Level Variables",
        variable %in% department_vars ~ "Department-Level Variables",
        variable %in% org_vars        ~ "Organisation-Level Variables",
        variable %in% policy_gender   ~ "Gender & Family Policies",
        variable %in% policy_age      ~ "Age-Related Policies"
      ))
  ) |>
  bold_labels() |>
  modify_header(
    label  = "**Variable**",
    stat_0 = "**Mean (SD) / N (%)**"
  ) |>
  modify_caption("**Table 1. Descriptive Statistics**") |>
  modify_footnote(
    stat_0 ~ "Continuous variables reported as Mean (SD); binary/categorical variables as N (%)."
  ) |>
  as_gt() |>
  gtsave("table1_descriptives.docx")

cat("table1_descriptives.docx saved.\n")
