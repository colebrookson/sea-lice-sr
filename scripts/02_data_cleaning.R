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

lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"

farm_data = read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations = read_csv(here("./data/raw/farm-locations.csv"))
lice_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
lice_site_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# ensure names of the same items are the same in both datasets
names(farm_data)
names(lice_data)
names(lice_site_data)

function(df) {

    # get current set of names
    current_names = names(df)
    # loop through, pull the name out, change "." to "_"
    for(name in seq_len(length(current_names))) {
        current_names[name] = gsub("\\.", "_", current_names[name])
    }
    # rename the dataframe
    names(df) = current_names


}

df = farm_data