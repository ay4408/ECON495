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
get_sumsts <- function (df) {
  df %>%
    select(where(is.numeric) & !rowid) %>%
    st(add.median = TRUE)
}

get_sumsts(IPEDS)

# 3.2 offrmbdcst might have outliers
ggplot(data = IPEDS) +
  geom_histogram(mapping = aes(x = offrmbdcst), binwidth = 1000) +
  coord_cartesian(ylim = c(0, 100))

# 3.3 change offrmbdcst outliers to NA
IPEDS <- IPEDS %>%
  mutate(offrmbdcst = ifelse(offrmbdcst == 0 | offrmbdcst > 90000, NA, offrmbdcst))

ggplot(data = IPEDS) +
  geom_histogram(mapping = aes(x = offrmbdcst), binwidth = 1000) +
  coord_cartesian(ylim = c(0, 100))
# 3.4 Investigate onrmbdcst, roomcap, pctadm and enrtot
get_sumsts(IPEDS)

ggplot(IPEDS) +
  geom_histogram(aes(onrmbdcst))

ggplot(IPEDS) +
  geom_histogram(aes(roomcap))

ggplot(IPEDS) +
  geom_histogram(aes(pctadm))

ggplot(IPEDS) +
  geom_histogram(aes(enrtot))

ggplot(IPEDS) +
  geom_histogram(aes(pctret))

IPEDS_cleaned <- IPEDS %>%
  mutate(avgintuit = ifelse(avgintuit == 0, NA, avgintuit),
         avgouttuit = ifelse(avgouttuit == 0, NA, avgouttuit),
         onrmbdcst = ifelse(onrmbdcst == 0, NA, onrmbdcst),
         offrmbdcst = ifelse(offrmbdcst == 0, NA, offrmbdcst),
         pctadm = ifelse(pctadm == 0, NA, pctadm))

get_sumsts(IPEDS_cleaned)
#------------------------------------------------
# 4. Export Data
#------------------------------------------------
saveRDS(IPEDS_cleaned, "./Data/IPEDS/Exported IPEDS Data/IPEDS_cleaned_3.rds")
