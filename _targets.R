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
             here::here("./data/farm-data/raw/BATI_farm_louse_data_RAW.csv"),
             format = "file"),
  tar_target(raw_open_dfo_data,
             here::here(
    "./data/farm-data/raw/gov-open-data/fish-farm-sea-louse-counts-data.csv"),
             format = "file"),
  tar_target(clean_open_dfo_data,
             clean_dfo_open_data(
               get_data_dfo_open(raw_open_dfo_data),
               here::here("./data/farm-data/clean/dfo-open-data-clean.csv")
             )),
  tar_target(marty_data,
             clean_data_marty(
               raw_data_file_marty,
               get_data_dfo_ref(dfo_farm_ref_data),
               here::here("./data/farm-data/clean/marty-data-clean.csv")),
             format = "rds"),
               #here::here("./data/farm-data/clean/marty-data-clean.csv"))),
  tar_target(bati_data,
             clean_data_bati(
               raw_data_file_bati,
               here::here("./data/farm-data/raw/farm-name-reference.csv"),
               here::here("./data/farm-data/clean/bati-data-clean.csv"))),
  tar_target(fill_in_missing_inventories,
             fill_in_missing_inventory_data(
               clean_open_dfo_data, 
               get_data_marty_cleaned(
                 here::here("./data/farm-data/clean/marty-data-clean.csv")),
               here::here(
                 "./data/farm-data/clean/missing-inventory-filled-data.csv")
             )),
  tar_target(fill_in_late_timeseries_inventories,
             match_inventory_data(
               bati_data,
               clean_open_dfo_data,
               here::here("./data/farm-data/clean/wakwa-tsaya-inventory.csv")
             ))
)
# tar_make(callr_function = NULL, names = any_of("marty_data"), 
#          shortcut = TRUE)