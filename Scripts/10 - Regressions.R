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
# install.packages("plm")
library(plm)
#install.packages("car")
library(car)

# 1.2 Check working directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load full analysis dataset
analysis_df <- read_rds("./Data/Analysis Datasets/analysis_dataset.rds")

# 2.2 Load subset of data (public/private 4-year colleges with enrollment that is 10-100% of population)
subset <- read_rds("./Data/Analysis Datasets/small_subset.rds")

count_full <- analysis_df %>%
  group_by(unitid) %>%
  count() %>%
  filter(n != 2)

count_subset <- subset %>%
  group_by(unitid) %>%
  count() %>%
  filter(n != 2)

#------------------------------------------------
# 3. Build models and run regressions
#------------------------------------------------
# 3.1 Simple regression on full data
m1 <- plm(estimate_medrentpctinc ~ student_to_pop,
                   data = analysis_df, index = c("unitid", "year"), model = "fd")
summary(m1)

# 3.2 Simple regression on subsetted data 
m2 <- plm(estimate_medrentpctinc ~ student_to_pop,
                     data = subset, index = c("unitid", "year"), model = "fd")
summary(m2)

# 3.3 Add controls for housing supply, number working outside of the county and unemployment on full
m3 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg + estimate_workoutcty +
                    unemp_rate,
                  data = analysis_df, index = c("unitid", "year"), model = "fd")
summary(m3)

# 3.4 Add controls for housing supply, number working outside of the county and unemployment on subset
m4 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg + estimate_workoutcty +
                      unemp_rate,
                    data = subset, index = c("unitid", "year"), model = "fd")
summary(m4)

# 3.5 Add controls for roomcap, selectivity and on-campus housing costs on full
m5 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg + estimate_workoutcty +
                       unemp_rate + roomcap + pctadm + onrmbdcst,
                     data = analysis_df, index = c("unitid", "year"), model = "fd")
summary(m5)

# 3.6 Add controls for roomcap, selectivity and on-campus housing costs on subset
m6 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg + estimate_workoutcty +
                       unemp_rate + roomcap + pctadm + onrmbdcst,
                     data = subset, index = c("unitid", "year"), model = "fd")
summary(m6)

# 3.7 Output regression tables
stargazer(m1, m2, m3, m4, m5, m6, 
          out = "Table Outputs/Regression Outputs/regressions.tex",
          title = "Regression Outputs",
          dep.var.labels = "median rent as percentage of income",
          covariate.labels = c("student share of population",
                               "total housing",
                               "no. working outside county",
                               "unemployment rate (county)",
                               "college housing capacity",
                               "pct. admitted",
                               "on-campus room & board cost"))
#------------------------------------------------
# 4. Summary Statistics 
#------------------------------------------------
# 4.1 Full Summary Statistics
sum_stats_full <- analysis_df %>%
  select(estimate_medrentpctinc, student_to_pop, roomcap, estimate_tothsg, estimate_workoutcty,
         unemp_rate, pctadm, onrmbdcst)
stargazer(as.data.frame(sum_stats_full),
          covariate.labels = c("median rent as pct. of income",
                               "student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside the county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on campus room & board cost"),
          out = "Table Outputs/Summary Statistics/sum_stats_full.tex")

# 4.2 Summary Statistics for subset
sum_stats_subset <- subset %>%
  select(estimate_medrentpctinc, student_to_pop, roomcap, estimate_tothsg, estimate_workoutcty,
         unemp_rate, pctadm, onrmbdcst)
stargazer(as.data.frame(sum_stats_subset),
          covariate.labels = c("median rent as pct. of income",
                               "student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside the county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on campus room & board cost"),
          out = "Table Outputs/Summary Statistics/sum_stats_subset.tex")

#------------------------------------------------
# 5. Analysis without outliers
#------------------------------------------------
no_outliers_full <- read_rds("./Data/Analysis Datasets/no_outliers_full.rds")
no_outliers_subset <- read_rds("./Data/Analysis Datasets/no_outliers_subset.rds")

missing_roomcap <- no_outliers_full %>%
  filter(is.na(roomcap))

msng_roomcap_subset <- no_outliers_subset %>%
  filter(is.na(roomcap))

count_fulloutlier <- no_outliers_full %>%
  group_by(unitid) %>%
  count() %>%
  ungroup() %>%
  filter(n != 2)

no_outliers_full <- no_outliers_full %>%
  group_by(unitid) %>%
  filter(!any(is.na(roomcap)))

count_subsetoutlier <- no_outliers_subset %>%
  group_by(unitid) %>%
  count() %>%
  ungroup() %>%
  filter(n != 2) %>%
  select(unitid) %>%
  as_vector()

no_outliers_subset <- no_outliers_subset %>%
  filter(!(unitid %in% count_subsetoutlier))

count_subsetoutlier <- no_outliers_subset %>%
  group_by(unitid) %>%
  count() %>%
  ungroup() %>%
  filter(n != 2) %>%
  select(unitid) %>%
  as_vector()

m7 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg +
            estimate_workoutcty + unemp_rate + roomcap + pctadm + onrmbdcst, 
          data = no_outliers_full, index = c("unitid", "year"), model = "fd")
summary(m7)

m8 <- plm(estimate_medrentpctinc ~ student_to_pop + estimate_tothsg + estimate_workoutcty +
            unemp_rate + roomcap + pctadm + onrmbdcst,
          data = no_outliers_subset, index = c("unitid", "year"), model = "fd")
summary(m8)

m9 <- plm(estimate_medrentpctinc ~ student_to_pop, 
          data = no_outliers_full, index = c("unitid", "year"), model = "fd")
summary(m9)

# + unemp_rate + estimate_tothsg + estimate_workcty +
# roomcap,

no_outliers_full <- no_outliers_full %>%
  mutate(pct_stdnt_hsng = roomcap / enrtot, .before = cstudent_to_pop)

no_outliers_subset <- no_outliers_subset %>%
  mutate(pct_stdnt_hsng = roomcap / enrtot, .before = cstudent_to_pop)

no_outliers_subset_2 <- no_outliers_subset %>%
  group_by(unitid) %>%
  filter(!any(is.na(roomcap))) %>%
  ungroup()

vif(m9)

m10 <- plm(estimate_medrentpctinc ~ student_to_pop + roomcap,
           data = no_outliers_subset, index = c("unitid", "year"), model = "fd")
summary(m10)

# Regressions on data without outliers
stargazer(m7, m8,
          out = "Table Outputs/Regression Outputs/regressions_no_outliers.tex",
          title = "Regression Outputs (without outliers)",
          dep.var.labels = "median rent as percentage of income",
          covariate.labels = c("student share of population",
                               "total housing",
                               "no. working outside county",
                               "unemployment rate (county)",
                               "college housing capacity",
                               "pct. admitted",
                               "on-campus room & board cost"))

# Full Summary Statistics
sum_stats_full_no_outliers <- no_outliers_full %>%
  select(estimate_medrentpctinc, student_to_pop, roomcap, estimate_tothsg, estimate_workoutcty,
         unemp_rate, pctadm, onrmbdcst)

stargazer(as.data.frame(sum_stats_full),
          covariate.labels = c("median rent as pct. of income",
                               "student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside the county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on campus room & board cost"),
          out = "Table Outputs/Summary Statistics/sum_stats_full_no_outliers.tex")

# Summary Statistics for subset
sum_stats_subset_no_outliers <- no_outliers_subset %>%
  select(estimate_medrentpctinc, student_to_pop, roomcap, estimate_tothsg, estimate_workoutcty,
         unemp_rate, pctadm, onrmbdcst)

stargazer(as.data.frame(sum_stats_subset),
          covariate.labels = c("median rent as pct. of income",
                               "student share of population",
                               "college housing capacity",
                               "total housing",
                               "no. working outside the county",
                               "unemployment rate (county)",
                               "pct. admitted",
                               "on campus room & board cost"),
          out = "Table Outputs/Summary Statistics/sum_stats_subset_no_outliers.tex")


# remove community colleges for sure
# remove bottom and top 1 percentile outliers
# make everything percentages for easier interpretation

