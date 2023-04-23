#=================================================================
# Program   :  09 - Explore_Analysis_Dataset.R
# Date      :  April 3, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Load Packages
library(tidyverse)
library(vtable)
library(stargazer)

# 1.2 Check Working Directory
getwd()

#------------------------------------------------
# 2. Load Dataset
#------------------------------------------------
# 2.1 Load data
analysis_df <- read_rds("./Data/Analysis Datasets/analysis_dataset.rds")

#------------------------------------------------
# 3. Subset data for separate analyses
#------------------------------------------------
# 3.1 Identify colleges with a student share less than 10% or greater than 100%
colleges_to_remove <- analysis_df %>%
  filter(!(10 <= student_to_pop & student_to_pop <= 100)) %>%
  select(unitid) %>%
  unique() %>%
  as_vector()

subset_df <- analysis_df %>%
  filter(!(unitid %in% colleges_to_remove))
# Still includes community/technical colleges

# Subset into public 4-yr, private 4-yr and both
count_subset <- subset_df %>%
  group_by(unitid) %>%
  count() %>%
  filter(n != 2)

pub_priv <- subset_df %>%
  group_by(unitid) %>%
  filter(any(sector == 1 | sector == 2)) %>%
  ungroup()

public <- subset_df %>%
  group_by(unitid) %>%
  filter(any(sector == 1)) %>%
  ungroup()

private <- subset_df %>%
  group_by(unitid) %>%
  filter(any(sector == 2)) %>%
  ungroup()

# 3.2 Transform regression variables for better interpretation
public <- public %>%
  mutate(uni_hsg_to_stdnt = 100 * (roomcap / enrtot),
         hsg_to_pop = 100 * (estimate_tothsg / estimate_totpop),
         pct_workoutcty = 100 * (estimate_workoutcty / estimate_totpop),
         lonrmbdcst = log(onrmbdcst),
         .after = estimate_medrentpctinc) %>%
  relocate(unemp_rate, pctadm, .after = lonrmbdcst)

private <- private %>%
  mutate(uni_hsg_to_stdnt = 100 * (roomcap / enrtot),
         hsg_to_pop = 100 * (estimate_tothsg / estimate_totpop),
         pct_workoutcty = 100 * (estimate_workoutcty / estimate_totpop),
         lonrmbdcst = log(onrmbdcst),
         .after = estimate_medrentpctinc) %>%
  relocate(unemp_rate, pctadm, .after = lonrmbdcst)

pub_priv <- pub_priv %>%
  mutate(uni_hsg_to_stdnt = 100 * (roomcap / enrtot),
         hsg_to_pop = 100 * (estimate_tothsg / estimate_totpop),
         pct_workoutcty = 100 * (estimate_workoutcty / estimate_totpop),
         lonrmbdcst = log(onrmbdcst),
         .after = estimate_medrentpctinc) %>%
  relocate(unemp_rate, pctadm, .after = lonrmbdcst)

# Function to difference variables
first_diff <- function(df, variable, new_variable_name) {
  df <- df %>%
    mutate(diff_variable = if_else(year == 2014,
                                     0,
                                     df[[variable]] - lag(df[[variable]])), .after = variable) %>%
    # Coerce values of difference in year 2014 to be NA
    mutate(diff_variable = replace(diff_variable, year == 2014, NA)) %>%
    rename_with(~ new_variable_name, diff_variable)
}

# 7.2 Create list of variables to difference
vars_to_diff <- pub_priv %>%
  select(student_to_pop:pctadm) %>%
  colnames()

diff_names <- paste0("c", vars_to_diff)

# 3.3 Difference
for (i in seq(vars_to_diff)) {
  pub_priv <- pub_priv %>%
    first_diff(vars_to_diff[i], diff_names[i])
}

for (i in seq(vars_to_diff)) {
  public <- public %>%
    first_diff(vars_to_diff[i], diff_names[i])
}

for (i in seq(vars_to_diff)) {
  private <- private %>%
    first_diff(vars_to_diff[i], diff_names[i])
}

#------------------------------------------------
# 4. Visualize Data
#------------------------------------------------
# 4.1 Create directory to store plot outputs
# dir.create("Plots")
# 4.2 Visualize relationship between dependent and main explanatory for each dataset
plot_pub_priv <- ggplot(data = pub_priv %>%
                           filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public and Private 4-year Colleges")
print(plot_pub_priv)
ggsave("./Plots/plot_pub_priv.png", plot = plot_pub_priv,
       width = 2400, height = 1600, units = "px")

plot_public <- ggplot(data = public %>%
                             filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public 4-year Colleges")
print(plot_public)
ggsave("./Plots/plot_public.png", plot = plot_public,
       width = 2400, height = 1600, units = "px")

plot_private <- ggplot(data = private %>%
                        filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Private 4-year Colleges")
print(plot_private)
ggsave("./Plots/plot_private.png", plot = plot_private,
       width = 2400, height = 1600, units = "px")

# Eliminate outliers
non_outlier_colleges <- pub_priv %>%
  filter(between(cstudent_to_pop, quantile(cstudent_to_pop, .01, na.rm = T), quantile(cstudent_to_pop, .99, na.rm = T))) %>%
  filter(between(cestimate_medrentpctinc, quantile(cestimate_medrentpctinc, .01, na.rm = T), quantile(cestimate_medrentpctinc, .99, na.rm = T))) %>%
  select(unitid) %>%
  as_vector()

diff_pub_priv_no <- pub_priv %>%
  filter(unitid %in% non_outlier_colleges)

diff_public_no <- public %>%
  filter(unitid %in% non_outlier_colleges)

diff_private_no <- private %>%
  filter(unitid %in% non_outlier_colleges)

# Plots
plot_private_no_outliers <- ggplot(data = diff_private_no %>%
                         filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Private 4-year Colleges (no outliers)")
print(plot_private_no_outliers)
ggsave("./Plots/plot_private_no_outliers.png", plot = plot_private_no_outliers,
       width = 2400, height = 1600, units = "px")

plot_public_no_outliers <- ggplot(data = diff_public_no %>%
                                     filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public 4-year Colleges (no outliers)")
print(plot_public_no_outliers)
ggsave("./Plots/plot_public_no_outliers.png", plot = plot_public_no_outliers,
       width = 2400, height = 1600, units = "px")

plot_pub_priv_no_outliers <- ggplot(data = diff_pub_priv_no %>%
                          filter(year == 2019)) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public and Private 4-year Colleges (no outliers)")
print(plot_pub_priv_no_outliers)
ggsave("./Plots/plot_pub_priv_no_outliers.png", plot = plot_pub_priv_no_outliers,
       width = 2400, height = 1600, units = "px")
#------------------------------------------------
# 5. Summary Statistics
#------------------------------------------------
# 5.1 Create directories to store outputs
# dir.create("Table Outputs")
# dir.create("Table Outputs/Regression Outputs")
# dir.create("Table Outputs/Summary Statistics")

#------------------------------------------------
# 7. Exported Data
#------------------------------------------------
# 7.1 Export subsets for regressions & summary statistics
# saveRDS(small_subset, "./Data/Analysis Datasets/small_subset.rds")
# saveRDS(no_outliers_full, "./Data/Analysis Datasets/no_outliers_full.rds")
# saveRDS(no_outliers_subset, "./Data/Analysis Datasets/no_outliers_subset.rds")
saveRDS(diff_private_no, "./Data/Analysis Datasets/private_colleges.rds")
saveRDS(diff_pub_priv_no, "./Data/Analysis Datasets/pubic_private_colleges.rds")
saveRDS(diff_public_no, "./Data/Analysis Datasets/public_colleges.rds")
