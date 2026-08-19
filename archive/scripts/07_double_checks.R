##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-21
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

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

# figure out which farms are NOT under BATI control ============================

# get the names of these farms and their locations
bati_farm_names = unique(bati_farms$farm)

