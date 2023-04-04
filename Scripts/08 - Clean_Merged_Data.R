#=================================================================
# Program   :  08 - Clean_Merged_Data.R
# Date      :  April 2, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Load/Install packages
library(tidyverse)
library(vtable)

# 1.2 Check working directory
getwd()

#------------------------------------------------
# 2. Load Data 
#------------------------------------------------
# 2.1 Load merged dataset
merged_data <- read_rds("./Data/Merged Datasets/merged_final.rds")

#------------------------------------------------
# 3. Explore Data
#------------------------------------------------
# 3.1 Generate summary statistics for numeric variables (2014)
#merged_data %>%
# filter(year == 2014) %>%
#  select(where(is.numeric) & !c(rowid, year)) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#              "pctile(x)[75]", "max(x)"),
#     summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                    "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                    "Max."),
#     title = "Summary Statistics (2014)")

# 3.2 Generate Summary statistics for numeric variables (2019)
#merged_data %>%
#  filter(year == 2019) %>%
#  select(where(is.numeric) & !c(rowid, year)) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#              "pctile(x)[75]", "max(x)"),
#     summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                    "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                    "Max."),
#     title = "Summary Statistics (2019)")

# 3.3 Generate summary statistics for all years
#merged_data %>%
#  select(where(is.numeric) & !rowid) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#              "pctile(x)[75]", "max(x)"),
#     summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                    "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                    "Max."),
#     title = "Summary Statistics")

#------------------------------------------------
# 4. Create new variables
#------------------------------------------------
# 4.1 Derive student to population ratio
merged_data <- merged_data %>%
  mutate(student_to_pop = round(enrtot / estimate_totpop, 4), .after = year)

# 4.2 Derive unemployment rate
merged_data <- merged_data %>%
  mutate(unemp_rate = round(estimate_unemp / estimate_lbrfrc, 2), .after = moe_unemp)

#------------------------------------------------
# 5. Relocate variables
#------------------------------------------------
# 5.1 Relocate variables to intuitive locations
merged_data <- merged_data %>%
  relocate(where(is.numeric) & !rowid, .after = year) %>%
  relocate(GEOID.city, .before = place) %>%
  relocate(unitid, .before = instnm) %>%
  relocate(roomcap:pctremo, .after = unemp_rate)

#------------------------------------------------
# 6. Log Variables
#------------------------------------------------
# 6.1 Function to log variables
# log_var <- function(df, column, new_column_name) {
#  df <- df %>%
#    mutate(new_column = log(df[[column]]), .after = column) %>%
#    rename_with(~ new_column_name, new_column)
# }

# 6.2 Log dependent and main explanatory variable
# vars_to_log <- merged_data %>%
#  select(student_to_pop:estimate_medrentpctinc) %>%
#  colnames()

# new_names <- paste0("l", vars_to_log)

#for (i in seq(vars_to_log)) {
 # merged_data <- merged_data %>%
#    log_var(vars_to_log[i], new_names[i])
# }

#------------------------------------------------
# 7. Difference Variables
#------------------------------------------------
# 7.1 Function to difference variables
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
vars_to_diff <- merged_data %>%
  select(student_to_pop:pctremo) %>%
  colnames()

diff_names <- paste0("c", vars_to_diff)

# 7.3 Difference
for (i in seq(vars_to_diff)) {
  merged_data <- merged_data %>%
    first_diff(vars_to_diff[i], diff_names[i])
}

# 7.4 Keep all values as whole numbers


#------------------------------------------------
# 8. Export Analysis Dataset
#------------------------------------------------
# 8.1 Create directory for analysis datasets
dir.create("./Data/Analysis Datasets")
# 8.2 Save data
saveRDS(merged_data, "./Data/Analysis Datasets/analysis_dataset.rds")