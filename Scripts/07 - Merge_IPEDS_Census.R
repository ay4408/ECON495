#=================================================================
# Program   :  07 - Merge_IPEDS_Census.R
# Date      :  March 30, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Install/Load Packages
library(tidyverse)

# 1.2 Check working directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Read in necessary files
census_places <- read_rds("./Data/Census/Exported Census Data/census_places_tidy_2.rds")
census_counties <- read_rds("./Data/Census/Exported Census Data/census_counties_tidy_2.rds")
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_3.rds")

#------------------------------------------------
# 3. Merge Data
#------------------------------------------------
# 3.1 Merge places
merge_test <- full_join(census_places, test_2) %>%
  filter(!is.na(instnm)) %>%
  relocate(c("instnm", "countynm"), .after = "GEOID") %>%
  arrange(instnm) 

# 3.2 Add counties
merge_test <- merge_test %>%
  rename(county = countynm)

merge_test_2 <- full_join(merge_test, census_counties)

# if university changed locations, dropped
# lots of errors to look at and fix