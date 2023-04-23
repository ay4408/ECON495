#=================================================================
# Program   :  05 - Clean_Census_Data.R
# Date      :  March 28, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Install/Load Packages
library(tidyverse)
# install.packages("vtable")
library(vtable)
library(openxlsx)

# 1.2 Check Working Directory
getwd()

#------------------------------------------------
# 2. Load Data 
#------------------------------------------------
# 2.1 Load tidied data on places and counties from Census
census_places_tidy <- read_rds("./Data/Census/Exported Census Data/census_places_tidy.rds")
census_counties_tidy <- read_rds("./Data/Census/Exported Census Data/census_counties_tidy.rds")

#------------------------------------------------
# 3. Clean Census places data
#------------------------------------------------
# 3.1 Get Summary Statistics of numerical variables
get_sumsts <- function (df) {
  df %>%
    select(where(is.numeric)) %>%
    st(add.median = TRUE)
}

get_sumsts(census_places_tidy)

# 3.3 Get all places by year
places_2014 <- census_places_tidy %>%
  filter(year == 2014)

places_2019 <- census_places_tidy %>%
  filter(year == 2019)

# 3.4 Return missing places in 2014
missing_places <- anti_join(places_2019, places_2014, by = "place")

# 3.5 Adjust dollar-denominated variables to account for inflation
# 3.5.1 Read in CPI data (downloaded from BLS)
CPI <- read.xlsx("r-cpi-u-rs-allitems.xlsx",
                 startRow = 8,
                 colNames = FALSE,
                 rowNames = FALSE,
                 cols = c(1,14)) %>%
  rename(year = X1, yravg = X2)

# 3.5.2 Filter to 2014 and 2019
CPI <- CPI %>%
  filter(year == 2014 | year == 2019)

# 3.5.3 Calculate inflation adj. factor
inf_adj_fctr = CPI$yravg[2] / CPI$yravg[1]

# 3.5.4 Apply inflation adj. factor to dollar-denominated values in 2014
census_places_tidy <- census_places_tidy %>%
  mutate(estimate_medhsgprc = if_else(year == 2014, 
                                      estimate_medhsgprc * inf_adj_fctr,
                                      estimate_medhsgprc),
         moe_medhsgprc = if_else(year == 2014,
                                 moe_medhsgprc * inf_adj_fctr,
                                 moe_medhsgprc),
         estimate_medinc = if_else(year == 2014,
                                   estimate_medinc * inf_adj_fctr,
                                   estimate_medinc),
         moe_medinc = if_else(year == 2014,
                              moe_medinc * inf_adj_fctr,
                              moe_medinc))

# 3.6 Check for missing values
sapply(census_places_tidy, function(x) sum(is.na(x)))
# 3.6.a Remove all cities without medrentpctinc (because it is main explanatory variable)
census_places_tidy <- census_places_tidy %>%
  group_by(GEOID) %>%
  filter(!any(is.na(estimate_medrentpctinc))) %>%
  ungroup()

# Check for missing values again
sapply(census_places_tidy, function(x) sum(is.na(x)))

# Get new summary statistics of numerical variables
get_sumsts(census_places_tidy)

#------------------------------------------------
# 4. Clean Census counties data
#------------------------------------------------
# 4.1 Get summary statistics of numerical variables
get_sumsts(census_counties_tidy)

# 4.2 Check for missing values
sapply(census_counties_tidy, function(x) sum(is.na(x)))

#------------------------------------------------
# 5. Export Data
#------------------------------------------------
saveRDS(census_places_tidy, "./Data/Census/Exported Census Data/census_places_cleaned.rds")
saveRDS(census_counties_tidy, "./Data/Census/Exported Census Data/census_counties_cleaned.rds")
