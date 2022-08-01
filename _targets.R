########## 
##########
# Targets file that has all defined targets to be used in this analysis pipeline
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-31
##########
##########

library(targets)
library(here)
source(here::here("./R/functions_data_cleaning.R"))
tar_option_set(packages = c("readr", "readexl", "here", "dplyr"))
list(
  tar_target(raw_marty_data, 
             here::here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
             format = "file"),
  tar_target(dfo_farm_ref_data,
             here::here("./data/farm-data/raw/farm-name-reference.csv"),
             format = "file"),
  tar_target(marty_data, 
             clean_marty_data(raw_marty_data,
                              2,
                              dfo_farm_ref_data,
       here::here("./data/farm-data/clean/marty-data-clean.csv")))
)
