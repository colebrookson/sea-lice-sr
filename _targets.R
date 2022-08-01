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
tar_option_set(packages = c("readr", "readxl", "here", "dplyr", "magrittr"))
list(
  tar_target(raw_data_file_marty,
             here::here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
             format = "file"),
  tar_target(dfo_farm_ref_data,
             here::here("./data/farm-data/raw/farm-name-reference.csv"),
             format = "file"),
  tar_target(raw_data_file_bati,
             here::here("./data/farm-data/raw/BATI_farm_louse_data_raw.csv"),
             format = "file"),
  tar_target(marty_data,
             clean_data_marty(
               raw_data_file_marty,
               2,
               dfo_farm_ref_data,
               here::here("./data/farm-data/clean/marty-data-clean.csv"))),
  tar_target(bati_data,
             clean_data_bati(
               raw_data_file_bati,
               here::here("./data/farm-data/raw/farm-name-reference.csv"),
               here::here("./data/farm-data/clean/bati-data-clean.csv")))
)

