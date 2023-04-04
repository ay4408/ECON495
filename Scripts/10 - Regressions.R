#=================================================================
# Program   :  10 - Regressions.R
# Date      :  April 3, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Load packages
library(tidyverse)
library(stargazer)

# 1.2 Check working directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load subset of analysis dataset
stdnts10pct_4yr <- read_rds("./Data/Analysis Datasets/stdnts10pct_4yr.rds") %>%
  # Filter to include only 2019 observations (for first-difference method)
  filter(year == 2019)

# 2.2 Load full analysis dataset
analysis_df <- read_rds("./Data/Analysis Datasets/analysis_dataset.rds") %>%
  filter(year == 2019)

#------------------------------------------------
# 3. Build models and run regressions
#------------------------------------------------
# 3.1 Basic model with all data
mlm_basic_full <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate), data = analysis_df)
summary(mlm_basic_full)
stargazer(mlm_basic_full, out = "Table Outputs/Regression Outputs/mlm_basic_full.tex")

# 3.2 Basic model with cities with 10% students and four-year colleges
mlm_basic_10pct4yr <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate), data = stdnts10pct_4yr)
summary(mlm_basic_10pct4yr)
stargazer(mlm_basic_10pct4yr, out = "Table Outputs/Regression Outputs/mlm_basic_10pct4yr.tex")

# 3.3 Add croomcap (all data)
mlm_roomcap_full <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + croomcap + 
                                         cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate), data = analysis_df)
summary(mlm_roomcap_full)
stargazer(mlm_roomcap_full, out = "Table Outputs/Regression Outputs/mlm_roomcap_full.tex")

# 3.4 croomcap (10pct4yr)
mlm_roomcap_10pct4yr <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + croomcap + 
                         cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate), data = stdnts10pct_4yr)
summary(mlm_roomcap_10pct4yr)
stargazer(mlm_roomcap_10pct4yr, out = "Table Outputs/Regression Outputs/mlm_roomcap_10pct4yr.tex")

# 3.5 Add controls for selectivity (cpctadm) and on-campus housing costs (conrmbdcst) (all data)
mlm_complete_full <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + croomcap + 
                 cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate) + I(100 * cpctadm) + conrmbdcst, data = analysis_df)
summary(mlm_complete_full)
stargazer(mlm_complete_full, out = "Table Outputs/Regression Outputs/mlm_complete_full.tex",
          title = "OLS Regression on Full Dataset",
          dep.var.labels = "median rent as percentage of income",
          covariate.labels = c("student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on-campus room & board cost"))

# 3.6 Apply full regression to smaller dataset
mlm_complete_10pct4yr <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop) + croomcap + 
                                 cestimate_tothsg + cestimate_workoutcty + I(100 * cunemp_rate) + I(100 * cpctadm) + conrmbdcst, data = stdnts10pct_4yr) 
stargazer(mlm_complete_10pct4yr, out = "Table Outputs/Regression Outputs/mlm_complete_10pct4yr.tex",
          title = "Complete Regression on Subset",
          dep.var.labels = "median rent as percentage of income",
          covariate.labels = c("student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on-campus room & board cost"))

#------------------------------------------------
# 4. Summary Statistics table for presentation
#------------------------------------------------
# 4.1 Full Summary Statistics
full_sum_stats_df <- analysis_df %>%
  select(cestimate_medrentpctinc, cstudent_to_pop, croomcap, cestimate_tothsg, cestimate_workoutcty,
         cunemp_rate, cpctadm, conrmbdcst) %>%
  mutate(cstudent_to_pop = 100 * cstudent_to_pop,
         cunemp_rate = 100 * cunemp_rate,
         cpctadm = 100 * cpctadm)
stargazer(as.data.frame(full_sum_stats_df), out = "Table Outputs/Summary Statistics/full_sum_stats_df.tex")

# 4.2 Summary Statistics for 10% 4-yr colleges/cities
sum_stats_10pct4yr <- stdnts10pct_4yr %>%
  select(cestimate_medrentpctinc, cstudent_to_pop, croomcap, cestimate_tothsg, cestimate_workoutcty,
         cunemp_rate, cpctadm, conrmbdcst) %>%
  mutate(cstudent_to_pop = 100 * cstudent_to_pop,
         cunemp_rate = 100 * cunemp_rate,
         cpctadm = 100 * cpctadm)
stargazer(as.data.frame(sum_stats_10pct4yr), out = "Table Outputs/Summary Statistics/sum_stats_10pct4yr.tex")
