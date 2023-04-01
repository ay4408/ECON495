library(tidyverse)

# read in distinct observations of city variable from dataset
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned.rds") 

IPEDS_cities <- IPEDS %>%
  select("city", "fips") %>%
  distinct() %>%
  mutate(fips = as.character(fips))

# read in fips codes
fips <- read_tsv("01 ALABAMA.txt") %>%
  mutate(fips = as.character(fips))

# match state names to cities by fips code
matched <- left_join(IPEDS_cities, fips, by = "fips") %>%
  arrange(state, city)

# add state names to IPEDS data frame
IPEDS_merged <- left_join(IPEDS, fips, by = "fips")
IPEDS_merged <- IPEDS_merged %>%
  relocate(state, .after = city)

# exported matched data
write_rds(IPEDS_merged, "./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_states.rds")