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

# set location of other data
lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"

# pull in data files 
farm_data_raw = read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations_raw = read_csv(here("./data/raw/farm-locations.csv"))
lice_data_raw = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
lice_site_data_raw = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# naming standardization =======================================================

names(farm_data)
names(lice_data)
names(lice_site_data)

farm_data = standardize_names(farm_data_raw)
lice_data = standardize_names(lice_data_raw)
lice_site_data = standardize_names(lice_site_data_raw)

# take average of information across temporal sampling period ==================

