library(tidyverse)

# read in distinct observations of city variable from dataset
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned.rds") %>%
  select("city", "fips") %>%
  distinct() %>%
  mutate(fips = as.character(fips))

# read in fips codes
fips <- read_tsv("01 ALABAMA.txt") %>%
  mutate(fips = as.character(fips))

# match state names to cities by fips code and delete fips column
matched <- left_join(IPEDS, fips, by = "fips") %>%
  mutate(fips = NULL) %>%
  arrange(state, city)

# export matched table as text file
write_tsv(matched, "cities.txt")
