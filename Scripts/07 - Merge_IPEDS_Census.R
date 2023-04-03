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
census_places <- read_rds("./Data/Census/Exported Census Data/census_places_to_merge.rds")
census_counties <- read_rds("./Data/Census/Exported Census Data/census_counties_to_merge.rds")
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_to_merge.rds")

#------------------------------------------------
# 3. Troubleshoot Merging Cities
#------------------------------------------------
# 3.1 Merge places
merge_test <- full_join(census_places, IPEDS, by = c("year" = "year",
                                                      "place" = "place",
                                                      "state" = "state")) %>%
  relocate(c("instnm", "county"), .after = "GEOID") %>%
  relocate("rowid", .before = "GEOID") %>%
  arrange(instnm) 

# 3.2 Identify colleges that were not matched
missing_colleges <- merge_test %>%
  filter(is.na(GEOID))

# 3.3 Identify cities that were not matched
missing_cities <- merge_test %>%
  filter(is.na(instnm))

census_places_raw <- read_rds("./Data/Census/census_cities_raw.rds")

# 3.4 Fix city names
census_places <- census_places %>%
  mutate(place = case_when(place == "Anchorage municipality" ~ "Anchorage",
                           place == "Alcorn State University" ~ "Alcorn State",
                           place == "Nashville-Davidson metropolitan government (balance)" ~ "Nashville",
                           place == "Amherst Center" ~ "Amherst",
                           place == "Athens-Clarke County unified government (balance)" ~ "Athens",
                           place == "Louisville/Jefferson County metro government (balance)" & state == "Kentucky" ~ "Louisville",
                           place == "Lexington-Fayette urban county" ~ "Lexington",
                           place == "Boise City" & state == "Idaho" ~ "Boise",
                           place == "Indianapolis (balance)" ~ "Indianapolis",
                           place == "Butte-Silver Bow (balance)" ~ "Butte",
                           place == "Macon-Bibb County" ~ "Macon",
                           place == "Port Hadlock-Irondale" ~ "Port Hadlock",
                           place == "Helena-West Helena" ~ "Helena",
                           TRUE ~ as.character(place)))

IPEDS <- IPEDS %>%
  mutate(place = case_when(place == "Saint Clairsville" & state == "Ohio" ~ "St. Clairsville",
                           place == "St Clairsville" & state == "Ohio" ~ "St. Clairsville",
                           instnm == "Bethel University" & state == "Minnesota" ~ "Arden Hills",
                           instnm == "Binghamton University" & state == "New York" ~ "Binghamton",
                           place == "Chestnut Hill" & state == "Massachusetts" ~ "Newton",
                           place == "West Barnstable" & state == "Massachusetts" ~ "Barnstable Town",
                           place == "Winston Salem" ~ "Winston-Salem",
                           place == "Honolulu" ~ "East Honolulu",
                           place == "Saint Martin" ~ "St. Martin",
                           place == "Cheney" & state == "Pennsylvania" ~ "Cheney University",
                           place == "South Abington Township" ~ "Clarks Summit",
                           place == "Saint Joseph" ~ "St. Joseph",
                           place == "Saint Bonifacius" ~ "St. Bonifacius",
                           place == "Franklin" & state == "Massachusetts" ~ "Franklin Town",
                           place == "Saint George" ~ "St. George",
                           place == "Saint Petersburg" ~ "St. Petersburg",
                           place == "Saint Augustine" ~ "St. Augustine",
                           place == "Saint Louis" ~ "St. Louis",
                           place == "Greenfield" & state == "Massachusetts" ~ "Greenfield Town",
                           place == "Saint Peter" ~ "St. Peter",
                           place == "Haverford" & state == "Pennsylvania" ~ "Haverford College",
                           place == "Vancleve" & state == "Kentucky" ~ "Jackson",
                           place == "La Plume" ~ "Factoryville",
                           place == "Lagrange" ~ "LaGrange",
                           place == "Sault Ste Marie" ~ "Sault Ste. Marie",
                           place == "Saint Charles" ~ "St. Charles",
                           place == "Purchase" & state == "New York" ~ "Harrison",
                           place == "Fond Du Lack" ~ "Fond du Lac",
                           place == "Wellesley Hills" ~ "Wellesley",
                           place == "North Andover" ~ "Andover",
                           place == "Saint Joseph" ~ "St. Joseph",
                           place == "Montclair" & state == "New Jersey" ~ "Upper Montclair",
                           str_detect(place, "Fort Morgan") ~ "Fort Morgan",
                           place == "South Hadley" ~ "Holyoke",
                           place == "Dekalb" & state == "Illinois" ~ "DeKalb",
                           place == "Espanola" ~ "Española",
                           place == "Hamden" & state == "Connecticut" ~ "North Haven",
                           place == "Mahwah" ~ "Ramsey",
                           place == "South Prince George" ~ "Prince George",
                           place == "Moon Township" ~ "Carnot-Moon",
                           place == "Rosemont" & state == "Pennsylvania" ~ "Bryn Mawr",
                           place == "Saint Cloud" ~ "St. Cloud",
                           place == "Saint Leo" ~ "St. Leo",
                           place == "Saint Mary of the Woods" ~ "St. Mary of the Woods",
                           place == "Mount Gay" ~ "Logan",
                           place == "Saint Bonaventure" ~ "St. Bonaventure",
                           place == "Merrimack" & state == "New Hampshire" ~ "East Merrimack",
                           place == "Toccoa Falls" ~ "Toccoa",
                           place == "Tougaloo" ~ "Jackson",
                           str_detect(place, "West Point") ~ "West Point",
                           place == "Anchorage Municipality" ~ "Anchorage",
                           place == "Juneau City and Borough" ~ "Juneau city and borough",
                           place == "North Dartmouth" & state == "Massachusetts" ~ "Smith Mills",
                           place == "Villanova" ~ "Bryn Mawr",
                           place == "Norton" & state == "Massachusetts" ~ "Norton Center",
                           str_detect(place, "Silver Spring") ~ "Silver Spring",
                           place == "South Fallsburgh" ~ "South Fallsburg",
                           TRUE ~ as.character(place)))

# 3.5 Try merging again
merge_test_2 <- full_join(census_places, IPEDS, by = c("year" = "year",
                                                       "place" = "place",
                                                       "state" = "state"))%>%
  relocate(c("instnm", "county"), .after = "GEOID") %>%
  relocate("rowid", .before = "GEOID") %>%
  arrange(instnm) 

# 3.6 Difference between first and second merge
merge_diffs <- anti_join(merge_test, merge_test_2)

# 3.7 Drop missing instnm values and missing GEOID variables to get merged dataset
merge_test_2 <- merge_test_2 %>%
  filter(!is.na(instnm)) %>%
  filter(!is.na(GEOID))

#------------------------------------------------
# 4. Troubleshoot Merging Counties
#------------------------------------------------
# 4.1 Merge counties
merge_test_3 <- full_join(merge_test_2, census_counties, by = c("year" = "year",
                                                                "county" = "county",
                                                                "state" = "state"),
                          suffix = c(".city", ".county"))

# 4.2 Drop counties without colleges
merged_final <- merge_test_3 %>%
  filter(!is.na(GEOID.city)) %>%
  relocate(GEOID.county, .before = "county")

#------------------------------------------------
# 5. Export Data
#------------------------------------------------
# 5.1 Create folder for merged data and save to RDS object
dir.create("./Data/Merged Datasets")
saveRDS(merged_final, "./Data/Merged Datasets/merged_final.rds")
