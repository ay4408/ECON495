#=================================================================
# Program   :  04 - Tidy_Census_Data.R
# Date      :  March 28, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Install/Load Packages
library(tidyverse)

# 1.2 Check Working Directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load in data frames
census_cities_raw <- read_rds("./Data/Census/census_cities_raw.rds")
census_counties_raw <- read_rds("./Data/Census/census_counties_raw.rds")

#------------------------------------------------
# 3. Tidy Data
#------------------------------------------------
# 3.1 Expand "variable" column to get individual variables per column
census_places_tidy <- census_cities_raw %>%
  arrange(NAME) %>%
  group_by(year) %>%
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe),
              names_vary = "slowest") %>%
  ungroup()

census_counties_tidy <- census_counties_raw %>%
  arrange(NAME) %>%
  group_by(year) %>%
  pivot_wider(names_from = variable,
              values_from = c(estimate, moe),
              names_vary = "slowest") %>%
  ungroup()

# 3.2 Separate NAME variable into county and state variables
census_counties_tidy <- census_counties_tidy %>%
  mutate(state = NAME %>%
           str_extract(",.+") %>%
           str_remove(", "), .after = NAME) %>%
  mutate(NAME = NAME %>%
           str_remove(",.+")) %>%
  rename(county = NAME)

census_places_tidy <- census_places_tidy %>%
  mutate(state = NAME %>%
           str_extract(",.+") %>%
           str_remove(", "), .after = NAME) %>%
  mutate(NAME = NAME %>%
           str_remove(",.+")) %>%
  rename(place = NAME)

# 3.3 Remove census place category labels
census_places_tidy <- census_places_tidy %>%
  mutate(place = place %>%
           str_remove(" city| CDP| town| borough| village"))

# 3.5 Load IPEDS data to check for wonky city names
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_2.rds")

# 3.5 Fix Augusta-Richmond County consolidated government
census_places_tidy <- census_places_tidy %>%
  mutate(place = if_else(place == "Augusta-Richmond County consolidated government (balance)",
                         "Augusta", place))

#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(census_places_tidy, "./Data/Census/Exported Census Data/census_places_tidy.rds")
saveRDS(census_counties_tidy, "./Data/Census/Exported Census Data/census_counties_tidy.rds")