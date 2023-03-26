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
IPEDS <- readRDS("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_states.rds")

# 3.2 Create master list to hold all cities
cities_by_state <- vector("list", 51)

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
}

#------------------------------------------------
# 4. Pull Data
#------------------------------------------------
# 4.1 Load variables for 2019 ACS 5-year estimates
v19 <- load_variables(2019, "acs5")
view(v19)

# 4.2 Pull data 
census_test <- get_acs(geography = "place",
                       variables = "B25008_003",
                       year = 2019,
                       state = "VA") %>%
  filter(str_detect(NAME, cities_by_state$VIRGINIA[[1]]))
