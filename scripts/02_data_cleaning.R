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

# farm data
farm_regress = farm_data %>% 
    select(lep_av, cal_av, farm, year, month)
readr::write_csv(farm_regress,
    here("./data/regression-data/farm-regression-data.csv"))

# look at ways lice are measured
scfs_lice_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid", "cal_cope", 
                    "cal_mot", "cal_gravid", "unid_cope", "chal_unid", 
                    "unid_pa", "unid_adult")
scfs_leps_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid")
scfs_cals = c("cal_cope", "cal_mot", "cal_gravid")

# sum across the lice spp cols -- INCLUDING CHALIMUS
scfs_data_chal_inc = scfs_data %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(all_lep = sum(lep_cope, lep_pamale, lep_pafemale, lep_male, 
                            lep_nongravid, lep_gravid, na.rm = TRUE),
                    all_cal = sum(cal_cope, cal_mot, cal_gravid,
                                    na.rm = TRUE), 
                    all_lice = sum(lep_cope, chala, chalb, lep_pamale, 
                                lep_pafemale, lep_male, lep_nongravid, 
                                lep_gravid, cal_cope, cal_mot, cal_gravid, 
                                unid_cope, chal_unid, unid_pa, unid_adult,
                                na.rm = TRUE))
# sum across the lice spp cols -- DO NOT INCLUDE CHALIMUS
scfs_data_chal_exc = scfs_data %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(all_lep = sum(lep_cope, lep_pamale, lep_pafemale, lep_male, 
                            lep_nongravid, lep_gravid, na.rm = TRUE),
                    all_cal = sum(cal_cope, cal_mot, cal_gravid,
                                    na.rm = TRUE), 
                    all_lice = sum(lep_cope, chala, chalb, lep_pamale, 
                                lep_pafemale, lep_male, lep_nongravid, 
                                lep_gravid, cal_cope, cal_mot, cal_gravid, 
                                unid_cope, chal_unid, unid_pa, unid_adult,
                                na.rm = TRUE))
# set up both data with only the information we want & write it out ============

# scfs data
scfs_regress_chal_inc = data.frame(scfs_data_chal_inc %>% 
    dplyr::select(all_lice, all_lep, all_cal, month, year, day, date, farm))

# need to make a column for week 
scfs_regress_chal_inc$week = 
    lubridate::week(lubridate::ymd(scfs_regress_chal_inc$date))

# make farm/week combo 
scfs_regress_chal_inc = scfs_regress_chal_inc %>% 
        dplyr::rowwise() %>%
        dplyr::mutate(farm_week = paste0(farm, week))

# write data
readr::write_csv(scfs_regress_chal_inc,
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))




# TUTORIAL: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/