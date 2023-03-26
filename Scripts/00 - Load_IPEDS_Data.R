#=================================================================
# Program   :  00 - Load_IPEDS_Data.R
# Date      :  March 24, 2023
# Project   :  ECON 495 Paper
# Author    :  Alex Yee
#=================================================================

#------------------------------------------------
# 1. Set Up Packages and Working Directory 
#------------------------------------------------
# 1.1 Install/Load Packages
library(tidyverse)

# 1.2 Set Working Directory
setwd('/Users/ay440/source/repos/ay4408/ECON495')
getwd()

#------------------------------------------------
# 2. Import Data 
#------------------------------------------------
# 2.1 Create list of zipped folder file paths
zipfolder_names <- list.files("./Data/IPEDS/IPEDS Data Raw", full.names = TRUE)

# 2.2 Create temporary directory to store extracted files
tmpdir <- tempdir()

# 2.3 Unzip files into temporary directory
all_files <- mapply(unzip, zipfile = zipfolder_names, exdir = tmpdir)

# 2.4 Create data frame to hold all .csv files
IPEDS_raw <- tibble()

# 2.5 Read in files and add to raw_df
for (i in 1:ncol(all_files)) {
  tempdf <- read_csv(all_files[1,i])
  IPEDS_raw <- bind_rows(IPEDS_raw, tempdf)
}

# 2.6 Create row ID
IPEDS_raw <- IPEDS_raw %>%
  mutate(rowid = row_number(), .before = unitid)
#------------------------------------------------
# 3. Export Data 
#------------------------------------------------
# 3.1 Create directory to store data frames
dir.create("./Data/IPEDS/Exported IPEDS Data")
# 3.2 Export data into Exported IPEDS Data Folder 
saveRDS(IPEDS_raw, file = "./Data/IPEDS/Exported IPEDS DATA/IPEDS_raw.rds")