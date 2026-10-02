########## 
##########
# Input inventories
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-26
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

# read in data on BATI farms
bati_farms = readr::read_csv(
  here::here(
    "./data/farm-data/raw/BATI_farm_louse_data_RAW_UPDATED20220716.csv"
  )
)

# read in manually collected information on farm locations
farm_locs = readr::read_csv(
  here::here("./data/farm-data/raw/farm-locations.csv")
)

# read in data from Marty 2010 (joined with BATI data)
marty_df = readr::read_csv(
  here::here("./data/farm-data/clean/marty-data.csv")
)

# get inventories that we have in hand =========================================

# get rid of months we don't want
marty_invent = marty_df %>% 
  dplyr::filter(month %in% c(3,4))

