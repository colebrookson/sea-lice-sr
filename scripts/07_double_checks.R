##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-21
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# figure out which farms are NOT under BATI control ============================

# read in data on BATI farms
bati_farms = readr::read_csv(
  here::here("./data/farm-data/raw/BATI_farm_louse_data_RAW.csv")
)

# get the names of these farms and their locations
bati_farm_names = 
  bati_farms %>% 
  dplyr::select(farm) %>% 
  unique()
