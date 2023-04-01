#=================================================================
# Program   :  06 - Prepare_to_Merge.R
# Date      :  April 1, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Load packages
library(tidyverse)

# 1.2 Check working directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load IPEDS Data
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_3.rds")

# 2.2 Load CPI Data
CPI <- read.xlsx("r-cpi-u-rs-allitems.xlsx",
                 startRow = 8,
                 colNames = FALSE,
                 rowNames = FALSE,
                 cols = c(1,14)) %>%
  rename(year = X1, yravg = X2) %>%
  filter(year == 2014 | year == 2019)

# 2.2 Load Census Data
census_places <- read_rds("./Data/Census/Exported Census Data/census_places_cleaned.rds")
census_counties <- read_rds("./Data/Census/Exported Census Data/census_counties_cleaned.rds")

#------------------------------------------------
# 3. Prepare IPEDS Data
#------------------------------------------------
# 3.1 Average IPEDS data (2010-2014) and (2015-2019)
# 3.1.1 Group data into year ranges
IPEDS <- IPEDS %>%
  mutate(year = as.numeric(levels(year))[year]) %>%
  mutate(yrgroup = ifelse(year >= 2010 & year <= 2014, 2014, 2019), .after = year) %>%
  mutate(yrgroup = as.factor(yrgroup),
         year = as.factor(year))

# 3.1.2 Function to calculate average percentages
avg_pct <- function(pct, tot) {
  round(sum(pct * tot, na.rm = T) / sum(tot, na.rm = T), 2)
}

IPEDS <- IPEDS %>%
  group_by(instnm, yrgroup) %>%
  mutate(pctadm = avg_pct(pctadm, applcn),
         pctret = avg_pct(pctret, enrft),
         pctyoung = avg_pct(pctret, efug),
         pctold = avg_pct(pctold, efug),
         pctoos = avg_pct(pctoos, efug1st),
         pctfrgn = avg_pct(pctfrgn, efug1st),
         pctremo = avg_pct(pctremo, enrtot)) %>%
  ungroup()

# 3.1.3 Calculate averages for other numerical variables
IPEDS <- IPEDS %>%
  group_by(instnm, yrgroup) %>%
  mutate(roomcap = mean(roomcap, na.rm = T),
         avgintuit = mean(avgintuit, na.rm = T),
         avgouttuit = mean(avgouttuit, na.rm = T),
         onrmbdcst = mean(onrmbdcst, na.rm = T),
         offrmbdcst = mean(offrmbdcst, na.rm = T),
         enrtot = mean(enrtot, na.rm = T),
         enrft = mean(enrft, na.rm = T),
         efug = mean(efug, na.rm = T),
         efug1st = mean(efug1st, na.rm = T),
         ugenrollft = mean(ugenrollft, na.rm = T),
         genrollft = mean(genrollft, na.rm = T)) %>%
  ungroup()

# 3.2 Coerce NaN values to NA (periods for which there is no data)
IPEDS <- IPEDS %>%
  mutate(across(everything(), ~replace(.x, is.nan(.x), NA)))

# 3.3 Keep only two observations (everything should already be
# averaged)
IPEDS <- IPEDS %>%
  filter(year == 2014 | year == 2019) %>%
  mutate(year = NULL) %>%
  rename(year = yrgroup)

# 3.4 Adjust dollar-denominated values for inflation
# 3.4.1 Calculate inflation adj. factor
inf_adj_fctr = CPI$yravg[2] / CPI$yravg[1]

# 3.4.2 Apply inflation adj. factor to dollar-denominated values in 2014
IPEDS <- IPEDS %>%
  mutate(avgintuit = if_else(year == 2014, 
                                      avgintuit * inf_adj_fctr,
                                      avgintuit),
         avgouttuit = if_else(year == 2014,
                                 avgouttuit * inf_adj_fctr,
                                 avgouttuit),
         onrmbdcst = if_else(year == 2014,
                                   onrmbdcst * inf_adj_fctr,
                                   onrmbdcst),
         offrmbdcst = if_else(year == 2014,
                              offrmbdcst * inf_adj_fctr,
                              offrmbdcst))

# 3.5 Rename IPEDS variables that are in Census
IPEDS <- IPEDS %>%
  rename(county = countynm,
         place = city)
#------------------------------------------------
# 4. Prepare Census Data
#------------------------------------------------
# 4.1 Convert year to factor
census_places <- census_places %>%
  mutate(year = as.factor(year))

census_counties <- census_counties %>%
  mutate(year = as.factor(year))

#------------------------------------------------
# 5. Export Data
#------------------------------------------------
saveRDS(census_counties, "./Data/Census/Exported Census Data/census_counties_to_merge.rds")
saveRDS(census_places, "./Data/Census/Exported Census Data/census_places_to_merge.rds")
saveRDS(IPEDS, "./Data/IPEDS/Exported IPEDS Data/IPEDS_to_merge.rds")