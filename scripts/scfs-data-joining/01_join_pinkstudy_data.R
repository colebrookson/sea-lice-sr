########## 
##########
# Pull in all data for cleaning 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# pull in file with all functions to clean data 
source(here("./src/02_data_cleaning_funs.R"))

pink_study = readr::read_csv(
    here("./data/louse-data/raw-scfs-data/pink_study_2001_data.csv")
)
scfs_regress = readr::read_csv(
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))
