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

source(here("./src/02_data_cleaning_funs.R"))

lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"

farm_data = read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations = read_csv(here("./data/raw/farm-locations.csv"))
lice_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
lice_site_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# naming standardization =======================================================
names(farm_data)
names(lice_data)
names(lice_site_data)

df = farm_data