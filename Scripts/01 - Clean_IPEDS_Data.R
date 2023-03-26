#=================================================================
# Program   :  01 - Clean_IPEDS_Data.R
# Date      :  March 24, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Install/Load Packages
# install.packages("expss")
library(expss)
library(tidyverse)

# 1.2 Check Working Directory
getwd()

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
IPEDS_raw <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_raw.rds")

#------------------------------------------------
# 3. Clean Data
#------------------------------------------------
# TODO: Look for erroneous values in data ESPECIALLY
# cities.

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
         xret_pcf = NULL) %>%
  rename(instnm = instnm...2,
         avgintuit = tuition2,
         avgouttuit = tuition3,
         onrmbdcst = chg5ay3,
         offrmbdcst = chg7ay3,
         pctadm = dvic01,
         region = obereg,
         titleiv = pset4flg,
         ugenroll = efugft,
         genroll = efgradft,
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
cols_to_factor <- c("year", "newid", "alloncam", "room", "instsize",
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
#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(IPEDS, "./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned.rds")