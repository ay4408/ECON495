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
                           place == "Augusta-Richmond County consolidated government (balance)" ~ "Augusta",
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
                           place == "Mississippi State" ~ "Starkville",
                           place == "Bridgewater" & state == "Massachusetts" & year == 2019 ~ "Bridgewater Town",
                           place == "Conception" & state == "Missouri" ~ "Conception Junction",
                           instnm == "Concordia University-Saint Paul" ~ "North St. Paul",
                           instnm == "Hamline University" & state == "Minnesota" ~ "North St. Paul",
                           instnm == "Macalester College" & state == "Minnesota" ~ "North St. Paul",
                           place == "Fond Du Lac" ~ "Fond du Lac",
                           place == "N Little Rock" ~ "North Little Rock",
                           place == "West  Point" ~ "West Point",
                           place == "Superior" ~ "Superior city",
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

# 3.7 Colleges that are still missing
missing_colleges_2 <- merge_test_2 %>%
  filter(is.na(GEOID)) %>%
  filter(!is.na(state))

#------------------------------------------------
# 4. Troubleshoot Merging Counties
#------------------------------------------------
# 4.1 Merge counties
merge_test_3 <- full_join(merge_test_2, census_counties, by = c("year" = "year",
                                                                "county" = "county",
                                                                "state" = "state"),
                          suffix = c(".city", ".county"))

# 4.2 Check missing colleges 
missing_colleges_3 <- merge_test_3 %>%
  filter(is.na(GEOID.county)) %>%
  filter(!is.na(state)) %>%
  filter(!is.na(unitid))

# Fix county names
merge_test_2 <- merge_test_2 %>%
  mutate(county = str_replace_all(county, " City", " city"))

# Check merge
merge_test_4 <- full_join(merge_test_2, census_counties, by = c("year" = "year",
                                                                "county" = "county",
                                                                "state" = "state"),
                          suffix = c(".city", ".county"))

missing_colleges_4 <- merge_test_4 %>%
  filter(is.na(GEOID.county)) %>%
  filter(!is.na(state)) %>%
  filter(!is.na(unitid))

# Fix county names
IPEDS <- IPEDS %>%
  mutate(county = case_when(county == "Marion County" & state == "Wisconsin" ~ "Fond du Lac County",
                            rowid == "649" ~ "Madison County",
                            rowid == "650" ~ "Anderson County",
                            rowid == "1960" ~ "Brooke County",
                            rowid == "1959" ~ "McPherson County",
                            rowid == "2013" ~ "St. Joseph County",
                            rowid == "2014" ~ "Ramsey County",
                            rowid == "2015" ~ "Carroll County",
                            rowid == "5404" ~ "Boone County",
                            rowid == "5405" ~ "Richland County",
                            rowid == "5403" ~ "Tuolumne County",
                            rowid == "7659" ~ "Franklin County",
                            rowid == "8865" ~ "Baldwin County",
                            rowid == "11515" ~ "Johnson County",
                            rowid == "12729" ~ "Cole County",
                            rowid == "12730" ~ "Chester County",
                            rowid == "17125" ~ "Montgomery County",
                            rowid == "21799" ~ "San Diego County",
                            rowid == "21800" ~ "Cowley County",
                            rowid == "22159" ~ "Anne Arundel County",
                            instnm == "St. John's College" & state == "New Mexico" ~ "Santa Fe County",
                            rowid == "22359" ~ "Rice County",
                            rowid == "22360" ~ "Orleans County",
                            rowid == "24273" ~ "Knox County",
                            rowid == "24274" ~ "Lancaster County",
                            rowid == "24275" ~ "Schenectady County",
                            instnm == "University of Alaska Southeast" ~ "Juneau City and Borough",
                            rowid == "25965" ~ "Dallas County",
                            rowid == "26550" ~ "Harris County",
                            rowid == "26549" ~ "Ramsey County",
                            county == "Carson city" ~ "Carson City",
                            rowid == "28193" ~ "Callaway County",
                            rowid == "28194" ~ "Lawrence County",
                            rowid == "28195" ~ "Salt Lake County",
                            rowid == "13509" ~ "Marion County",
                            rowid == "15805" | rowid == "15825" ~ "Dona Ana County",
                            TRUE ~ as.character(county)))

census_counties <- census_counties %>%
  mutate(county = str_replace_all(county, " city", " City"))

# 4.3 Try merging again
merge_test_5 <- full_join(census_places, IPEDS, by = c("year" = "year",
                                                       "place" = "place",
                                                       "state" = "state"))%>%
  relocate(c("instnm", "county"), .after = "GEOID") %>%
  relocate("rowid", .before = "GEOID") %>%
  arrange(instnm)  

merge_test_5 <- full_join(merge_test_5, census_counties, by = c("year" = "year",
                                                                "county" = "county",
                                                                "state" = "state"),
                          suffix = c(".city", ".county"))

# Check missing
missing_colleges_5 <- merge_test_5 %>%
  filter(is.na(GEOID.county)) %>%
  filter(!is.na(state)) %>%
  filter(!is.na(unitid))

merged_final <- merge_test_5 %>%
  relocate(unitid, .before = instnm)

sapply(merged_final, function(x) sum(is.na(x)))

# 4.2 Drop unmatched observations
merged_final <- merged_final %>%
  filter(!is.na(GEOID.city) & !is.na(GEOID.county)) %>%
  relocate(GEOID.county, .before = "county")

sapply(merged_final, function(x) sum(is.na(x)))

# 4.2.b Check for uneven observations of colleges
uneven_colleges <- merged_final %>%
  filter(!is.na(unitid)) %>%
  add_count(unitid) %>%
  filter(n != 2) %>%
  select(unitid) %>%
  as_vector()

# 4.2 Drop uneven colleges
merged_final <- merged_final %>%
  filter(!(unitid %in% uneven_colleges))

# 4.2 Check for uneven observations
uneven_2 <- merged_final %>%
  filter(!is.na(unitid)) %>%
  add_count(unitid) %>%
  filter(n != 2)
# 0 observations

#------------------------------------------------
# 5. Export Data
#------------------------------------------------
# 5.1 Create folder for merged data and save to RDS object
dir.create("./Data/Merged Datasets")
saveRDS(merged_final, "./Data/Merged Datasets/merged_final.rds")
