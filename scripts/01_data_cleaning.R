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
farm_file_location = "./data/raw-farm/canadian-gov-open-data/"

# pull in data files 
farm_data_raw = readr::read_csv(here(
    "./data/raw-farm/BATI_farm_louse_data_RAW.csv"))
farm_locations_df = readr::read_csv(here("./data/raw-farm/farm-locations.csv"))
raw_marty_data = readxl::read_excel(
    path = here("./data/raw-farm/marty-2010-data/sd01.xlsx"),
    sheet = 2
)
dfo_open_data = read_csv(here(paste0(farm_file_location, 
                                "fish-farm-sea-louse-counts-data.csv")))
scfs_data_raw = readr::read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
lice_site_data_raw = readr::read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))


# naming standardization =======================================================

names(farm_data_raw)
names(scfs_data_raw)
names(lice_site_data_raw)

bati_df = standardize_names(farm_data_raw)
scfs_data = standardize_names(scfs_data_raw)
lice_site_data = standardize_names(lice_site_data_raw)

# change "location" to farm for the lice data 
names(scfs_data)[names(scfs_data) == "location"] = "farm"


# Gary Mary PNAS (2010) data processing ========================================

##### BEGIN NOTE #######################
# Note that the data in this section are downloaded directly from the related 
# PNAS paper (https://doi.org/10.1073/pnas.1009573108) (we downloaded and store
# the data instead of including a `wget` or the like for simplicity).
##### END NOTE #######################

# use previously written functions to clean up the data 
marty_df = raw_marty_data %>% 

    # step 1 -  select only columns of interest, chop the bottom summary stuff 
    # off, and rename the columns 
    trim_marty_data() %>% 

    # step 2 - add in farm names
    farm_names() %>% 

    # step 3 - rename the months to the number of the month 
    fix_months()

# filter out times when stocks were empty (i.e. # of fish is zero)
marty_df_stocked = marty_df %>% 
    dplyr::filter(!is.na(inventory))

# ensure that farm names/numbers are matching to map ===========================

# put together relevant data
farm_loc = bind_map_data(raw_marty_data, farm_locations_df, dfo_open_data,
                            c("Wicklow Point", "Burdwood", "Glacier Falls (1)"))

# write out combos of farm names and farm numbers 
farm_map_nums = farm_loc %>% 
    # keep relevant columns
    dplyr::select(farm_name, farm_num) %>% 
    # ensure both galcier falls are categorized together 
    dplyr::rowwise() %>% 
    dplyr::mutate(
        farm_name = ifelse(farm_name %in% 
                c("Glacier Falls (1)", "Glacier Falls (2)"),
            "Glacier Falls", farm_name),
        farm_num = ifelse(farm_name == "Glacier Falls", 6, farm_num)
    ) %>% 
    unique()
readr::write_csv(farm_map_nums, 
        here("./data/clean-farm/farm-numbers-names-according-to-map.csv"))

# bind marty and bati data =====================================================

all_farm_data = join_marty_bati_data(
    marty_data_trimmed, farm_data, farm_map_nums
)
readr::write_csv(
    all_farm_data, here("./data/clean-farm/marty-bati-joined.csv")
)

# check that there are no lep_tot measures when there is not a stock measurement
for (row in seq_len(nrow(all_farm_data))) { 

    temp_row = all_farm_data[row, c("inventory", "lep_av", "lep_tot")]

    # make sure at least one of lep_av or inventory is NA if lep_tot is NA
    if (
        is.na(temp_row$lep_tot) &
            (!is.na(temp_row$inventory) & !is.na(temp_row$lep_av))
    ) {
        stop(paste0("ERROR - PROBLEM AT ROW NUMBER ", row, 
            "lep_tot cannot be NA if there are inventory and average values!"))
    }

    # if there is a lep_tot value, make sure neither lep_av or inventory is NA
    if (
        !is.na(temp_row$lep_tot) & 
            (is.na(temp_row$inventory) | is.na(temp_row$lep_av))
    ) {
        stop(paste0("ERROR - PROBLEM AT ROW NUMBER ", row, 
            " --lep_tot cannot have value if inventory or avg are NA!"))
    }


}
if (!identical(sort(names(marty_df_deferred)), 
            sort(names(bati_df_renamed)))) {

    stop("column names not the same!")
}

# add option that excludes non-stocked obs/no lep count obs 


all_farm_stocked = all_farm_data %>% 

    # filter inventoryt and lep_tot 
    dplyr::filter(!is.na(lep_tot))

# prepare data sources for regression ==========================================



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

# exclude data from weeks 28, 33, 9 
scfs_regress_chal_inc = scfs_regress_chal_inc %>% 
    dplyr::filter(week %notin% c(28, 33, 9))

# write data
readr::write_csv(scfs_regress_chal_inc,
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))
##################### BELOW THIS POINT IS NOT CLEAN ###########################


# open government data processing ==============================================
farm_data = farm_data %>% 
    dplyr::filter(Finfish Aquaculture Reporting Zone)









# Updated data from public source ==============================================
updated_farm_data = read_csv(here(
    "./data/raw-farm/canadian-gov-open-data/fish-farm-sea-louse-counts-data.csv"
))


# take average of info across temporal sampling period =========================

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







# TUTORIAL: https://benalexkeen.com/creating-a-timeline-graphic-using-r-and-ggplot2/