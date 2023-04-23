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

# Remove raw data frames
rm(census_cities_raw, census_counties_raw)

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

# Check for missing place or state values
sapply(census_counties_tidy, function(x) sum(is.na(x)))
sapply(census_places_tidy, function(x) sum(is.na(x)))

# 3.3 Remove census place category labels
census_places_tidy_nolabs <- census_places_tidy %>%
  mutate(place = place %>%
           str_remove(" city| CDP| town| borough| village"))

# Identify matching values within states
check_matching <- census_places_tidy_nolabs %>%
  add_count(state, place, name = "dupes") %>%
  filter(dupes > 2)
# 385 "duplicates" by state

# 3.4 Create vector of names that should not be de-labeled
dup_GEOID <- check_matching %>%
  distinct(GEOID) %>%
  select(GEOID) %>%
  as_vector()

# 3.5 Remove census place category labels EXCEPT for specified places
census_places_tidy <- census_places_tidy %>%
  mutate(place = 
           if_else(GEOID %in% dup_GEOID, place, str_remove(place, " city| CDP| town| borough| village")))

#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(census_places_tidy, "./Data/Census/Exported Census Data/census_places_tidy.rds")
saveRDS(census_counties_tidy, "./Data/Census/Exported Census Data/census_counties_tidy.rds")