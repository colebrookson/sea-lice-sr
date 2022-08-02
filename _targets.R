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
library(tarchetypes)
library(here)

source(here::here("./R/functions_farm_data_cleaning.R"))
source(here("./R/functions_general_data_cleaning.R"))
tar_option_set(packages = c("readr", "here", "dplyr", "magrittr"))
list(
  tar_target(raw_data_file_marty,
             here::here(
               "./data/farm-data/raw/marty-2010-data/marty-data-copied.csv"),
             format = "file"),
  tar_target(dfo_farm_ref_data,
             here::here("./data/farm-data/raw/farm-name-reference.csv"),
             format = "file"),
  tar_target(raw_data_file_bati,
             here::here("./data/farm-data/raw/BATI_farm_louse_data_raw.csv"),
             format = "file"),
  tar_target(raw_open_dfo_data,
             here::here(
               paste0("./data/farm-data/raw/canadian-gov-open-data/",
                  "fish-farm-sea-louse-counts-data.csv")),
             format = "file"),
  tar_target(marty_data,
             clean_data_marty(
               raw_data_file_marty,
               get_data_dfo_ref(dfo_farm_ref_data)),
             format = "rds"),
               #here::here("./data/farm-data/clean/marty-data-clean.csv"))),
  tar_target(bati_data,
             clean_data_bati(
               raw_data_file_bati,
               here::here("./data/farm-data/raw/farm-name-reference.csv"),
               here::here("./data/farm-data/clean/bati-data-clean.csv"))),
  tar_target(fill_in_missing_inventories,
             fill_in_missing_inventory_data(
               raw_open_dfo_data, 
               marty_data
             ))
)
# tar_make(callr_function = NULL, names = any_of("marty_data"), 
#          shortcut = TRUE)