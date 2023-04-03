#=================================================================
# Program   :  08 - Explore_Merged_Data.R
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
library(stargazer)

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
log_var <- function(df, column, new_column_name) {
  df <- df %>%
    mutate(new_column = log(df[[column]]), .after = column) %>%
    rename_with(~ new_column_name, new_column)
}

# 6.2 Log dependent and main explanatory variable
vars_to_log <- merged_data %>%
  select(student_to_pop:estimate_medrentpctinc) %>%
  colnames()

new_names <- paste0("l", vars_to_log)

for (i in seq(vars_to_log)) {
  merged_data <- merged_data %>%
    log_var(vars_to_log[i], new_names[i])
}

#------------------------------------------------
# 7. Difference Variables
#------------------------------------------------
# 7.1 Function to difference variables
first_diff <- function(df, variable, new_variable_name) {
  df <- df %>%
    mutate(diff_variable = if_else(year == 2014,
                                     0,
                                     df[[variable]] - lag(df[[variable]])), .after = variable) %>%
    rename_with(~ new_variable_name, diff_variable)
}

# 7.2 Create list of variables to difference
vars_to_diff <- merged_data %>%
  select(student_to_pop:pctremo) %>%
  colnames()

diff_names <- paste0("c", vars_to_diff)

# Filter to include only 4-year public colleges outside of large cities
public_4year <- test %>%
  filter(sector == 1,
         locale == 13 | locale == 21 | locale == 22
         | locale == 23 | locale == 31 | locale == 32 |
           locale == 33 | locale == 41 | locale == 42 |
           locale == 43)

# Filter to include only colleges w/ 10% of population
many_students <- test %>%
  mutate(student_to_pop = enrtot / estimate_totpop) %>%
  filter(student_to_pop > .1)

many_students <- many_students %>%
  log_var("student_to_pop", "log_student_to_pop") 


many_students <- many_students %>%
  mutate(diff_rentpctinc = if_else(year == 2014,
                                   0,
                                   log_medrentpctinc - lag(log_medrentpctinc)),
         diff_student_to_pop = if_else(year == 2014,
                              0,
                              log_student_to_pop - lag(log_student_to_pop))) %>%
  mutate(diff_rentpctinc = na_if(diff_rentpctinc, 0),
         diff_student_to_pop = na_if(diff_student_to_pop, 0))
  
# 3.6 Create dataset of variables of interest
enrl_rent <- test %>%
  select(instnm, year, diff_rentpctinc, diff_enrft) %>%
  filter(!is.na(diff_rentpctinc)) %>%
  filter(!is.na(diff_enrft))

enrl_rent_public <- public_4year %>%
  select(instnm, year, diff_rentpctinc, diff_enrft) %>%
  filter(!is.na(diff_rentpctinc)) %>%
  filter(!is.na(diff_enrft))

enrl_rent_many_students <- many_students %>%
  select(instnm, year, diff_rentpctinc, diff_student_to_pop) %>%
  filter(!is.na(diff_rentpctinc)) %>%
  filter(!is.na(diff_student_to_pop))

# Create scatterplots of variables of interest
ggplot(data = enrl_rent) +
  geom_point(mapping = aes(x = diff_enrft, y = diff_rentpctinc))

ggplot(data = enrl_rent_public) +
  geom_point(mapping = aes(x = diff_enrft, y = diff_rentpctinc))

ggplot(data = enrl_rent_many_students) +
  geom_point(mapping = aes(x = diff_student_to_pop, y = diff_rentpctinc))

# 3.8 Create directories to store outputs
# dir.create("Table Outputs")
# dir.create("Table Outputs/Regression Outputs")
# dir.create("Table Outputs/Summary Statistics")

# 3.9 Run Regressions
# all colleges
lm1 <- lm(diff_rentpctinc ~ diff_enrft, data = enrl_rent)
summary(lm1)
stargazer(lm1, out = "Table Outputs/Regression Outputs/lm1.txt")

# public 4-year colleges outside large cities
lm2 <- lm(diff_rentpctinc ~ diff_enrft, data = enrl_rent_public)
summary(lm2)
stargazer(lm2, out = "Table Outputs/Regression Outputs/lm2.txt")

# many students
lm3 <- lm(diff_rentpctinc ~ diff_student_to_pop, data = enrl_rent_many_students)
summary(lm3) 
stargazer(lm3, out = "Table Outputs/Regression Outputs/lm3.txt")
