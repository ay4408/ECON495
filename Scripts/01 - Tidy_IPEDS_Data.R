#=================================================================
# Program   :  01 - Tidy_IPEDS_Data.R
# Date      :  March 24, 2023
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
IPEDS_raw <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_raw_2.rds")

#------------------------------------------------
# 3. Clean Data
#------------------------------------------------
# 3.1 Drop old institution name and rename variables
IPEDS <- IPEDS_raw %>%
  mutate(instnm...4 = NULL, 
         deathyr = NULL,
         # Drop imputation flags
         xroomcap = NULL,
         xtuit2 = NULL,
         xtuit3 = NULL,
         xchg5ay3 = NULL,
         xchg7ay3 = NULL,
         xret_pcf = NULL,
         # drop newid variable
         newid = NULL) %>%
  rename(instnm = instnm...2,
         avgintuit = tuition2,
         avgouttuit = tuition3,
         onrmbdcst = chg5ay3,
         offrmbdcst = chg7ay3,
         pctadm = dvic01,
         region = obereg,
         titleiv = pset4flg,
         ugenrollft = efugft,
         genrollft = efgradft,
         pctret = ret_pcf,
         pctyoung = dvef14,
         pctold = dvef15,
         pctoos = rmousttp,
         pctfrgn = rmfrgncp,
         pctremo = pctdeexc)
 
# 3.2 Copy values from dvadm01 to pctadm and delete dvadm01
IPEDS <- IPEDS %>% 
  mutate(pctadm = if_else(is.na(pctadm), dvadm01, pctadm)) %>%
  mutate(dvadm01 = NULL)

# 3.3 Arrange by college by year
IPEDS <- IPEDS %>%
  arrange(instnm, year) %>%
  # Update rowid
  mutate(rowid = row_number())

# 3.4 Coerce factor variables
cols_to_factor <- c("year", "alloncam", "room", "instsize",
                    "fips", "region", "sector", "iclevel",
                    "control", "deggrant", "locale", "titleiv")

IPEDS[,cols_to_factor] <- lapply(IPEDS[cols_to_factor], as.factor)

# 3.5 Coerce to percentages
# Create function to divide by 100 (for use with dplyr)
convert_to_pct <- function (x) {
  x / 100
}

IPEDS <- IPEDS %>%
  mutate(across(starts_with("pct"), convert_to_pct))

# 3.6 Coerce to characters
cols_to_chr <- c("unitid", "countycd")

IPEDS[,cols_to_chr] <- lapply(IPEDS[cols_to_chr], as.character)

# 3.7 Move geographic variables to one group
IPEDS <- IPEDS %>%
  relocate(c("stabbr", "fips", "region", "locale"), .after = countynm) %>%
  relocate(c("applcn", "pctadm"), .after = genrollft)

# 3.8 Standardize countynm variable
IPEDS <- IPEDS %>%
  mutate(countynm = countynm %>%
           str_remove(",.+")) %>%
  mutate(countynm = if_else((year == 2014) & (!is.na(lag(countynm))), lead(countynm), countynm))

# 3.9 Fix erroneous city values
# 3.9.a Check for strange city names
dup <- IPEDS %>%
  filter(city == "University" | str_detect(city, "University"))
# 3.9.b Change city values (used internet for correct city names)
IPEDS <- IPEDS %>%
  mutate(city = case_when(city == "Auburn University" ~ "Auburn",
                          city == "Niagara University" ~ "Lewiston",
                          city == "University" ~ "Oxford",
                          city == "University of Richmond" ~ "Richmond",
                          TRUE ~ as.character(city))) 

# 3.10 Change state abbreviation to full name
# 3.10.a Load in table of fips codes and state names
state_names <- read_tsv("01 Alabama.txt") %>%
  mutate(fips = as.factor(fips))

# 3.10.b Change abbreviations to full names
IPEDS <- left_join(IPEDS, state_names) %>%
  mutate(stabbr = state, state = NULL) %>%
  rename(state = stabbr)
#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(IPEDS, "./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_2.rds")
