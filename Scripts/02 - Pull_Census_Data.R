#=================================================================
# Program   :  03 - Pull_Census_Data.R
# Date      :  March 25, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set up Packages
#------------------------------------------------
# 1.1 Install and Load Packages
# install.packages("tidycensus")
library(tidycensus)
library(tidyverse)

#------------------------------------------------
# 2. Prepare for working with Census API
#------------------------------------------------
# 2.1 Set census API key 
# register for key at (http://api.census.gov/data/key_signup.html)
# set key (on first time, add install = TRUE)
census_api_key("88b41677ffc27c7960073bc1b0a2e01ab29ced6b")

#------------------------------
# FOR FIRST TIME:
# readRenviron("~/.Renviron")
# check that key is stored
# Sys.getenv("CENSUS_API_KEY")
#------------------------------

#------------------------------------------------
# 3. Create lists of cities by state
#------------------------------------------------
# 3.1 Load IPEDS data frame
IPEDS <- readRDS("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_2.rds")

# 3.2 Create master list to hold all cities
cities_by_state <- vector("list", 56)

# 3.3 Create vector of alphabetically sorted state names 
state_names <- IPEDS %>%
  select(state) %>%
  distinct() %>%
  filter(!is.na(state)) %>%
  arrange(state) %>%
  as_vector()

# 3.4 Name each list element as a state
names(cities_by_state) <- state_names

# 3.5 Insert list of unique cities by state into master list
for (i in seq_along(state_names)) {
  state_cities <- IPEDS %>%
    filter(state == state_names[i]) %>%
    select(city) %>%
    distinct()
  
  cities_by_state[[i]] <- as.list(state_cities$city)
  rm(state_cities)
}

#------------------------------------------------
# 4. Pull Data
#------------------------------------------------
# 4.1 Create dataframe of variables and labels
variables <- c("B25071_001", "B25077_001", "B01001_001", "B06011_001", 
               "B23025_005", "B23025_003", "B25003_001",
               "B25003_003", "B25003_002", "B08007_003",
               "B08007_004")
variable_names <- c("medrentpctinc", "medhsgprc", "totpop", "medinc", "unemp", "lbrfrc", "tothsg", "renthsg", "ownhsg", "workcty",
                    "workoutcty")

census_vars <- tibble(var = variables, var_name = variable_names)

# 4.2 Query Census API
# function to query Census API based on geography and year
query_API <- function(geo, yr) {
  if(geo == "place") {
    df <- get_acs(geography = "place",
            variables = c(medrentpctinc = variables[1],
                          medhsgprc = variables[2],
                          totpop = variables[3],
                          medinc = variables[4],
                          tothsg = variables[7],
                          renthsg = variables[8],
                          ownhsg = variables[9],
                          workcty = variables[10],
                          workoutcty = variables[11]),
            year = yr,
            state = state_names)
  }
  else if (geo == "county") {
    df <- get_acs(geography = "county",
            variables = c(unemp = variables[5],
                          lbrfrc = variables[6]),
            year = yr,
            state = state_names)
  }
  
}

city_level_2014 <- query_API("place", 2014) %>%
  mutate(year = 2014)
county_level_2014 <- query_API("county", 2014) %>%
  mutate(year = 2014)
city_level_2019 <- query_API("place", 2019) %>%
  mutate(year = 2019)
county_level_2019 <- query_API("county", 2019) %>% 
  mutate(year = 2019)

# 4.4 Merge Census data into two data frames for export
census_cities <- bind_rows(city_level_2014, city_level_2019)
census_counties <- bind_rows(county_level_2014, county_level_2019)

#------------------------------------------------
# 5. Export Data
#------------------------------------------------
saveRDS(census_cities, "./Data/Census/census_cities_raw.rds")
saveRDS(census_counties, "./Data/Census/census_counties_raw.rds")
saveRDS(census_vars, "./Data/Census/census_vars.rds")
saveRDS(cities_by_state, "./Data/Census/cities_by_state.rds")
