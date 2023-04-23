#=================================================================
# Program   :  11 - Regressions_2.R
# Date      :  April 21, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Load Packages
library(tidyverse)
library(stargazer)
library(plm)

# 1.2 Check working directory 
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load datasets
public_colleges <- read_rds("./Data/Analysis Datasets/public_colleges.rds")
private_colleges <- read_rds("./Data/Analysis Datasets/private_colleges.rds")
public_private_colleges <- read_rds("./Data/Analysis Datasets/pubic_private_colleges.rds")

# 2.2 Relocate variables
public_colleges <- public_colleges %>%
  relocate(cstudent_to_pop,
             cestimate_medrentpctinc,
             cuni_hsg_to_stdnt,
             chsg_to_pop,
             cpct_workoutcty,
             clonrmbdcst,
             cunemp_rate,
             cpctadm, .after = pctadm)

private_colleges <- private_colleges %>%
  relocate(cstudent_to_pop,
           cestimate_medrentpctinc,
           cuni_hsg_to_stdnt,
           chsg_to_pop,
           cpct_workoutcty,
           clonrmbdcst,
           cunemp_rate,
           cpctadm, .after = pctadm)

public_private_colleges <- public_private_colleges %>%
  relocate(cstudent_to_pop,
           cestimate_medrentpctinc,
           cuni_hsg_to_stdnt,
           chsg_to_pop,
           cpct_workoutcty,
           clonrmbdcst,
           cunemp_rate,
           cpctadm, .after = pctadm)
#------------------------------------------------
# 3. Summary Statistics
#------------------------------------------------
stargazer(as.data.frame(public_colleges %>%
                          select(cstudent_to_pop:cpctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/public_sumst.tex")

stargazer(as.data.frame(private_colleges %>%
                          select(cstudent_to_pop:cpctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/private_sumst.tex")

stargazer(as.data.frame(public_private_colleges %>%
                          select(cstudent_to_pop:cpctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/public_private_sumst.tex")

stargazer(as.data.frame(public_private_colleges %>%
                          select(student_to_pop:pctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/public_private_sumst_static.tex")

stargazer(as.data.frame(public_colleges %>%
                          select(student_to_pop:pctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/public_sumst_static.tex")

stargazer(as.data.frame(private_colleges %>%
                          select(student_to_pop:pctadm)),
          covariate.labels = c("student share of pop.",
                               "median rent as pct. of inc.",
                               "college housing per student",
                               "city housing per inhabitant",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "county unemployment rate",
                               "pct. admitted"),
          out = "Table Outputs/Summary Statistics/private_sumst_static.tex")
#------------------------------------------------
# 4. Regressions
#------------------------------------------------
# 4.1 Simple regressions
m1 <- plm(estimate_medrentpctinc ~ student_to_pop, 
          data = public_private_colleges, index = c("unitid", "year"), model = "fd")
summary(m1)

m2 <- plm(estimate_medrentpctinc ~ student_to_pop, 
          data = public_colleges, index = c("unitid", "year"), model = "fd")
summary(m2)

m3 <- plm(estimate_medrentpctinc ~ student_to_pop, 
          data = private_colleges, index = c("unitid", "year"), model = "fd")
summary(m3)

# 4.2 Add controls
m4 <- plm(estimate_medrentpctinc ~ student_to_pop + uni_hsg_to_stdnt +
            hsg_to_pop + pct_workoutcty + lonrmbdcst + unemp_rate + pctadm,
          data = public_private_colleges, index = c("unitid", "year"), model = "fd")
summary(m4)

m5 <- plm(estimate_medrentpctinc ~ student_to_pop + uni_hsg_to_stdnt +
            hsg_to_pop + pct_workoutcty + lonrmbdcst + unemp_rate + pctadm,
          data = public_colleges, index = c("unitid", "year"), model = "fd")
summary(m5)

m6 <- plm(estimate_medrentpctinc ~ student_to_pop + uni_hsg_to_stdnt +
            hsg_to_pop + pct_workoutcty + lonrmbdcst + unemp_rate + pctadm,
          data = private_colleges, index = c("unitid", "year"), model = "fd")
summary(m6)

# 4.3 Test for serial correlation in the error term
pbgtest(m1)
pbgtest(m2)
pbgtest(m3)
pbgtest(m4)
pbgtest(m5)
pbgtest(m6)

# 4.4 Output regression results
stargazer(m1, m4, m2, m5, m3, m6,
          out = "Table Outputs/Regression Outputs/regressions_2.tex",
          title = "Regression Outputs",
          dep.var.labels = "median rent as percentage of income",
          covariate.labels = c("student share of population",
                               "university housing units per student",
                               "city housing per resident",
                               "pct. working outside county",
                               "log(on-campus room and board cost)",
                               "unemployment rate (county)",
                               "pct. admitted"),
          column.labels = c("Public/Private 4-year",
                              "Public 4-year",
                              "Private 4-year"),
          column.separate = c(2, 2, 2))
