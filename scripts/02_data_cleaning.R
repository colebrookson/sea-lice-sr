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
farm_data_raw = readr::read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations_raw = readr::read_csv(here("./data/raw/farm-locations.csv"))
scfs_data_raw = readr::read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
lice_site_data_raw = readr::read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# naming standardization =======================================================

names(farm_data_raw)
names(scfs_data_raw)
names(lice_site_data_raw)

farm_data = standardize_names(farm_data_raw)
scfs_data = standardize_names(scfs_data_raw)
lice_site_data = standardize_names(lice_site_data_raw)

# change "location" to farm for the lice data 
names(scfs_data)[names(scfs_data) == "location"] = "farm"

# take average of information across temporal sampling period ==================

# TUTORIAL: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/

# make date at first of every month or at the day given 
farm_data$date_first = with(farm_data, 
                    lubridate::ymd(sprintf("%d-%02d-%02d", year, month, 1)))
scfs_data$date = with(scfs_data,
                    lubridate::ymd(sprintf("%d-%02d-%02d", year, month, day)))

# group by columns we want and keep the unique ones 
timeline_farm = farm_data %>% 
    dplyr::select(farm, date_first) %>%
    dplyr::filter(farm %in% c("Burdwood",
                                "Glacier Falls",
                                "Wicklow Point")) %>%
    dplyr::group_by(date_first, farm) %>%
    dplyr::mutate(data = "farm") %>%
    dplyr::rename(date = date_first) %>% 
    unique()
# remove the extra words for easier comparison 
timeline_farm[which(timeline_farm$farm == "Glacier Falls"),"farm"] = "Glacier"
timeline_farm[which(timeline_farm$farm == "Wicklow Point"),"farm"] = "Wicklow"

# group for the salmon coast data too
timeline_scfs = scfs_data %>% 
    dplyr::select(farm, date) %>%
    dplyr::group_by(farm, date) %>%
    dplyr::mutate(data = "scfs") %>%
    unique()

# bind these data together
timeline_data = rbind(timeline_farm, timeline_scfs)



# put two data sources together for regression =================================

# look at ways lice are measured
scfs_lice_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid", "cal_cope", 
                    "cal_mot", "cal_gravid", "unid_cope", "chal_unid", 
                    "unid_pa", "unid_adult")
scfs_leps_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid")
scfs_cals = c("cal_cope", "cal_mot", "cal_gravid")

# sum across the lice spp cols
scfs_data = scfs_data %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(all_lep = sum(lep_cope, lep_pamale, lep_pafemale, lep_male, 
                            lep_nongravid, lep_gravid, na.rm=TRUE),
                    all_cal = sum(cal_cope, cal_mot, cal_gravid, 
                                    na.rm = TRUE), 
                    all_lice = sum(lep_cope, chala, chalb, lep_pamale, 
                                lep_pafemale, lep_male, lep_nongravid, 
                                lep_gravid, cal_cope, cal_mot, cal_gravid, 
                                unid_cope, chal_unid, unid_pa, unid_adult,
                                na.rm = TRUE))

# create long version of the scfs data for plotting in an overlay
scfs_lice_long = data.frame(lice_counts = rbind(scfs_data$all_lep, 
                            scfs_data$all_cal, 
                            scfs_data$all_lice), 
                            lice_type = c(rep("all", nrow(scfs_data)), 
                                rep("cals", nrow(scfs_data)), 
                                rep("leps", nrow(scfs_data))))
head(scfs_lice_long)
hist(scfs_data$lice_all)
ggplot(data = scfs_data) + 
    geom_overlay()

