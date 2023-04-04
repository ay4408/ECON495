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
# 3.1 Filter to include only colleges where students are at least 10% of population
students_10pct <- analysis_df %>%
  group_by(instnm) %>%
  filter(student_to_pop >= .1) %>%
  ungroup()

# 3.2 Filter to include only public/private 4-year colleges (sector = 1 (public), sector = 2 (private))
colleges_4yr <- analysis_df %>%
  group_by(instnm) %>%
  filter(sector == 1 | sector == 2) %>%
  ungroup()

# 3.3 Filter using both above criteria
stdnts10pct_4yr <- analysis_df %>%
  group_by(instnm) %>%
  filter((student_to_pop >= .1 & (sector == 1 | sector == 2))) %>%
  ungroup()

#------------------------------------------------
# 4. Visualize Data
#------------------------------------------------
# 4.1 Create directory to store plot outputs
# dir.create("Plots")
# 4.2 Visualize relationship between dependent and main explanatory for each dataset
students_10pct_plot <- ggplot(data = students_10pct) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Cities with >= 10% student population")
print(students_10pct_plot)
ggsave("./Plots/students_10pct_plot.tex", plot = students_10pct_plot, device = "tex")

colleges_4yr_plot <- ggplot(data = colleges_4yr) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public/Private 4-year colleges")
print(colleges_4yr_plot)
ggsave("./Plots/colleges_4yr_plot.tex", plot = colleges_4yr_plot, device = "tex")

stdnts_10pct_4yr_plot <- ggplot(data = stdnts10pct_4yr) +
  geom_point(mapping = aes(x = cstudent_to_pop, y = cestimate_medrentpctinc)) +
  labs(x = "change in student to population ratio",
       y = "change in median rent as pct. of income",
       title = "Public/Private 4-year colleges (enrollment >= 10% of city pop.)")
print(stdnts_10pct_4yr_plot)
ggsave("./Plots/stdnts_10pct_4yr_plot.tex", plot = stdnts_10pct_4yr_plot, device = "tex")
#------------------------------------------------
# 5. Summary Statistics
#------------------------------------------------
# 5.1 Create directories to store outputs
# dir.create("Table Outputs")
# dir.create("Table Outputs/Regression Outputs")
# dir.create("Table Outputs/Summary Statistics")

# 5.2 Generate summary statistics for two datasets for now
# students_10pct (2014)
#students_10pct %>%
#  filter(year == 2014) %>%
#  select(where(is.numeric) & !c(rowid, year)) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#               "pctile(x)[75]", "max(x)"),
#    summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                     "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                     "Max."),
##    title = "Summary Statistics students_10pct (2014)",
#   out = "latex",
#    file = "./Table Outputs/Summary Statistics/students_10pct_sumstats_2014")
# students_10pct (2019)
#students_10pct %>%
#  filter(year == 2019) %>%
# select(where(is.numeric) & !c(rowid, year)) %>%
# st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#             "pctile(x)[75]", "max(x)"),
#    summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                   "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                   "Max."),
#    title = "Summary Statistics students_10pct (2019)",
#    out = "latex",
#    file = "./Table Outputs/Summary Statistics/students_10pct_sumstats_2019")
#
# stdnts_10pct_4yr (2014)
#stdnts10pct_4yr %>%
#  filter(year == 2014) %>%
#  select(where(is.numeric) & !c(rowid, year)) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#              "pctile(x)[75]", "max(x)"),
#     summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                    "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                    "Max."),
#     title = "Summary Statistics stdnts_10pct_4yr (2014)",
#     out = "latex",
#     file = "./Table Outputs/Summary Statistics/stdnts_10pct_4yr_sumstats_2014")
# students_10pct_4yr (2019)
#stdnts10pct_4yr %>%
#  filter(year == 2019) %>%
#  select(where(is.numeric) & !c(rowid, year)) %>%
#  st(summ = c("notNA(x)", "countNA(x)", "mean(x)", "sd(x)", "min(x)", "pctile(x)[25]", "pctile(x)[50]",
#              "pctile(x)[75]", "max(x)"),
#     summ.names = c("N", "No. Missing", "Mean", "SD", "Min.",
#                    "Pctl. 25", "Pctl. 50", "Pctl. 75",
#                    "Max."),
#     title = "Summary Statistics stdnts_10pct_4yr (2019)",
#     out = "latex",
#     file = "./Table Outputs/Summary Statistics/stdnts_10pct_4yr_sumstats_2019")

#------------------------------------------------
# 6. Simple Regressions
#------------------------------------------------
# 6.1 All colleges
reg_all_colleges <- analysis_df %>%
  filter(year == 2019)

lm_all_colleges <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop), reg_all_colleges)
summary(lm_all_colleges)
stargazer(lm_all_colleges, out = "Table Outputs/Regression Outputs/lm_all_colleges.tex")

# 6.2 Cities with 10% or greater student population

# 6.3 Public/Private 4-year colleges

# 6.4 10% or greater student pop. and public/private 4-year 
reg_stdnts10pct_4yr <- stdnts10pct_4yr %>%
  filter(year == 2019)

lm_stdnts10pct_4yr <- lm(cestimate_medrentpctinc ~ I(100 * cstudent_to_pop), reg_stdnts10pct_4yr)
summary(lm_stdnts10pct_4yr)
stargazer(lm_stdnts10pct_4yr, out = "Table Outputs/Regression Outputs/lm_stdnts10pct_4yr.tex")

#------------------------------------------------
# 7. Exported Data
#------------------------------------------------
# 7.1 Export Subsetted analysis dataset
saveRDS(stdnts10pct_4yr, "./Data/Analysis Datasets/stdnts10pct_4yr.rds")
