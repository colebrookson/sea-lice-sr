library(here)
library(tidyverse)
source("./R/functions_farm_data_cleaning.R")

clean_data_marty(
 raw_file = here::here("./data/farm-data/raw/marty-2010-data/marty-data-copied.csv"),
 dfo_file = here::here("./data/farm-data/raw/farm-name-reference.csv"),
 output_path = here::here("./data/farm-data/clean/marty-data-clean.csv")
)


bati_df = get_bati_data(here::here(
 "./data/farm-data/raw/BATI_farm_louse_data_raw.csv"))
bati_df = standardize_names(bati_df)
dfo_names = get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv"))
bati_names = farm_names_bati(bati_df, dfo_names)
write_data_bati(bati_names, here::here("./data/farm-data/clean/bati-data-cleaned.csv"))
bati_df %>%
 standardize_names() %>%
 farm_names_bati(.,  get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv"))) %>%
 write_data_bati(., here::here("./data/farm-data/clean/bati-data-cleaned.csv"))

clean_data_bati(
  here::here("./data/farm-data/raw/BATI_farm_louse_data_raw.csv"),
  get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv")),
  here::here("./data/farm-data/clean/bati-data-cleaned.csv"))
