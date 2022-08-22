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

source(here::here("./R/functions_plot_themes.R"))
source(here::here("./R/functions_farm_data_cleaning.R"))
source(here::here("./R/functions_general_data_cleaning.R"))
source(here::here("./R/functions_wild_lice_cleaning.R"))
source(here::here("./R/functions_wild_lice_regression.R"))

tar_option_set(packages = c("readr", "here", "dplyr", "magrittr", "ggthemes", 
                            "ggplot2", "betareg"))
list(
  ####################
  # files
  ####################
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
  tar_target(scfs_raw_data_file,
             here::here(
               paste0("./data/wild-lice-data/raw/Sea-lice-database-master/",
                      "Data/BroughtonSeaLice_fishData.csv")),
             format = "file"),
  ####################
  # cleaning data
  ####################
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
             )),
  tar_target(join_all_farm_data,
             join_farm_data(
               bati_data,
               get_data_marty_cleaned(
                 here::here("./data/farm-data/clean/marty-data-clean.csv")),
               fill_in_missing_inventories,
               fill_in_late_timeseries_inventories,
               here::here("./data/farm-data/clean/all-farms-joined-clean.csv")
             )),
  tar_target(prepare_wild_lice_data,
             clean_data_scfs(
               get_data_scfs(scfs_raw_data_file),
               here::here("./data/wild-lice-data/clean/scfs-data-clean.csv")
             )),
  ####################
  # wild lice regressions
  ####################
  tar_target(motile_proportions_logistic_regression,
             lep_regression_mot(
               prepare_wild_lice_data,
               here::here("./outputs/model-outputs/mot-regression/")
             )),
  tar_target(cope_proportions_logistic_regression,
             lep_regression_cope(
               prepare_wild_lice_data,
               here::here("./outputs/model-outputs/cope-regression/")
             )),
  tar_target(wild_lice_nonlinear_regression,
             nonlinear_regression_scenario(
               prepare_wild_lice_data,
               here::here("./outputs/model-outputs/nonlinear-regression/")
             )),
  tar_target(wild_lice_beta_regression,
             beta_regression_scenario(
               prepare_wild_lice_data,
               here::here("./outputs/model-outputs/nonlinear-regression/")
             ))
  
)
