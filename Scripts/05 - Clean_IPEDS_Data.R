#=================================================================
# Program   :  03 - Pull_Census_Data.R
# Date      :  March 25, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set up Packages
#------------------------------------------------
# 1.1 Load packages
library(tidyverse)
library(vtable)

#------------------------------------------------
# 2. Load Data
#------------------------------------------------
# 2.1 Load saved data frame
IPEDS <- read_rds("./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_2.rds")

#------------------------------------------------
# 3. Explore Data
#------------------------------------------------
# 3.1 Get Summary Statistics of numerical variables
get_sumsts <- function () {
  IPEDS %>%
    select(where(is.numeric) & !rowid) %>%
    st(add.median = TRUE)
}

# 3.2 offrmbdcst might have outliers
ggplot(data = IPEDS) +
  geom_histogram(mapping = aes(x = offrmbdcst), binwidth = 1000) +
  coord_cartesian(ylim = c(0, 100))

# 3.3 change offrmbdcst outliers to NA
IPEDS <- IPEDS %>%
  mutate(offrmbdcst = ifelse(offrmbdcst == 0 | offrmbdcst > 90000, NA, offrmbdcst))

# 3.4 avgintuit, avgouttuit, onrmbdcst, pctadm should all not have 0 by definition
get_sumsts()
IPEDS <- IPEDS %>%
  mutate(avgintuit = ifelse(avgintuit == 0, NA, avgintuit),
         avgouttuit = ifelse(avgouttuit == 0, NA, avgouttuit),
         onrmbdcst = ifelse(onrmbdcst == 0, NA, onrmbdcst),
         pctadm = ifelse(pctadm == 0, NA, pctadm))

get_sumsts()

# 3.5 Drop colleges with NA or 0 full-time enrollment in ANY year
IPEDS <- IPEDS %>%
  group_by(instnm) %>%
  filter(!any(is.na(enrft))) %>%
  filter(enrft != 0) %>%
  ungroup()

get_sumsts()

# 3.5 Drop colleges with missing states (Puerto Rican colleges)
IPEDS <- IPEDS %>%
  group_by(instnm) %>%
  filter(!any(is.na(state))) %>%
  ungroup()

#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(IPEDS, "./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_3.rds")
