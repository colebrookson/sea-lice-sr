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
library(zoo)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

# set location of other data
lice_file_location = "./data/wild-lice-data/Sea-lice-database-master/Data/"
farm_file_location = "./data/farm-data/raw/canadian-gov-open-data/"

# pull in data files 
farm_data_raw = readr::read_csv(here(
    "./data/farm-data/raw/BATI_farm_louse_data_RAW_UPDATED20220716.csv"))
farm_locations_df = readr::read_csv(
  here("./data/farm-data/raw/farm-locations.csv"))
raw_marty_data = readxl::read_excel(
    path = here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
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

# write out Marty data now that it's cleaned
readr::write_csv(marty_df,
  here::here("./data/farm-data/clean/marty-data.csv")
)

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
        here("./data/farm-data/clean/farm-numbers-names-according-to-map.csv")
)

# bind marty and bati data =====================================================

all_farm_data = join_marty_bati_data(
    marty_df, bati_df, farm_map_nums
)
readr::write_csv(
    all_farm_data, here("./data/farm-data/clean/marty-bati-joined.csv")
)

# check that there are no lep_tot measures when there is not a stock measurement
check_lep_total_calculations(all_farm_data)

# add option that excludes non-stocked obs/no lep count obs 
all_farm_data_stocked = all_farm_data %>% 
    # exclude where lep_tot is NA
    dplyr::filter(!is.na(lep_tot))

### NOTE ########
# there are a number of farms outside of BATI control that we don't have 
# inventory data for. Four of those farms have inventory data in the 
# Marty dataset, so we can use the inventories in that dataset to to infer the 
# monthly inventories in the years after 2010 and then use DFO data for the 
# lice numbers. Here, I'll calculate thsoe values, pair them with the DFO 
# data of average lice values, and then bind that with the 
# `all_farm_data_stocked` dataframe I've already created then write that out
### END NOTE ########

# get names of the farms we have inventory for in the Marty df
missing_farms = c("Maude Island", "Whlis Bay", "Simmonds Point",
                  "Noo-la")

# get those inventories
missing_inventories = marty_df %>% 
  dplyr::filter(farm_name %in% missing_farms) %>% 
  dplyr::select(inventory, month, year, farm_name) %>% 
  dplyr::rowwise()

# put in date
missing_inventories$date = zoo::as.yearmon(
  paste0(missing_inventories$year, 
        missing_inventories$month), "%Y %m")

# make new values for average monthly inventories for each farm 
avg_missing_inventories = missing_inventories %>% 
  dplyr::group_by(month, farm_name) %>% 
  dplyr::summarize(mean_inventory = mean(inventory, na.rm = TRUE)) %>% 
  dplyr::filter(month %in% c(3,4))

# write out values
readr::write_csv(
  avg_missing_inventories,
  here::here("./data/farm-data/clean/missing-farms-average-inventories.csv")
)
  
# plot to visualize
missing_inventories_plot = ggplot(data = missing_inventories) + 
  geom_line(aes(x = date, y = inventory, colour = farm_name)) + 
  geom_point(aes(x = date, y = inventory, fill = farm_name),
             colour = "black", shape = 21) + 
  theme_bw()
ggplot2::ggsave(
  here::here("./figs/missing-inventory-farms-timeline-pre-2010.png"),
  missing_inventories_plot,
  height = 6, width = 7
)

readr::write_csv(
    all_farm_data_stocked, 
        here("./data/farm-data/clean/marty-bati-data-joined-stocked-only.csv")
)

# prepare data sources for regression ==========================================

# look at ways lice are measured
scfs_lice_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid", "cal_cope", 
                    "cal_mot", "cal_gravid", "unid_cope", "chal_unid", 
                    "unid_pa", "unid_adult")
scfs_leps_cols = c("lep_cope", "chala", "chalb", "lep_pamale", "lep_pafemale",
                    "lep_male", "lep_nongravid", "lep_gravid")
scfs_cals = c("cal_cope", "cal_mot", "cal_gravid")

# do regression like in Bateman et al. (2016)
mot_data = scfs_data %>% 
    dplyr::rowwise() %>%
    dplyr::mutate( # make columns that divide the lice into species 
        all_lep = sum(lep_pamale, lep_pafemale, lep_male, 
                            lep_nongravid, lep_gravid, unid_pa, 
                            na.rm = TRUE),
        all_cal = sum(cal_mot, cal_gravid,
                                    na.rm = TRUE), 
        all_lice = sum(lep_pamale, 
                                lep_pafemale, lep_male, lep_nongravid, 
                                lep_gravid, cal_mot, cal_gravid,
                                unid_adult, unid_pa, 
                                na.rm = TRUE),
        date = lubridate::make_date(year, month, day)) %>% 
    dplyr::select( # keep only the columns we want
        year, all_lep, all_lice
    ) %>%
    dplyr::group_by(
        year
    ) %>% 
    dplyr::summarize( # find yearly means for the two we want
        mean_lep = mean(all_lep, na.rm = TRUE),
        mean_all = mean(all_lice, na.rm = TRUE)
    ) %>% 
    dplyr::filter( # keep this out since it's getting predicted
        year != 2001
    ) %>% 
    dplyr::mutate( # find the proportion of all the lice that are leps
        prop_lep = mean_lep / mean_all
    )

# non-linear regression (asymptotic) to get the shape of the curve
# fit with Y = a - (a - b) * exp(-c * X) 
# note that the value for a is fixed at 1.0 since it's an actual hard 
# asymptote
model = stats::nls(
    formula = prop_lep ~ 1.0 - (1.0 - 0.0) * exp(- c * mean_all), 
    start = list(c = 2), 
    data = mot_data)

# predict the data back from the model 
pred_data_points = data.frame(
    mean_all = seq(0,4,0.01)
)
pred_prop = stats::predict(model, 
    pred_data_points,
    type = "response")
predicted_line = data.frame(cbind(pred_data_points, pred_prop)) %>% 
    dplyr::rename(prop_lep = pred_prop)

# make data for a plot 
pred_data_all_points = rbind( # do this for the points 
        data.frame(year = 2001,
            mean_all = 3.44, 
            # get the predicted value for the missing
            prop_lep = 
                predicted_line[which(
                    predicted_line$mean_all == 3.44), "prop_lep"]
        ),
        data.frame(mot_data %>% 
            dplyr::select(-c(mean_lep)) 
        )
    ) %>% 
    mutate(
        predicted = c("Predicted", rep("True", 20))
    ) 
pred_data_all_points$predicted = as.factor(pred_data_all_points$predicted)

predicted_proportions = ggplot() +
    geom_point(data = pred_data_all_points, 
        aes(x = mean_all, y = prop_lep, fill = predicted),
        shape = 21, size = 2.8) + 
    geom_text(data = pred_data_all_points, 
        aes(x = mean_all, y = prop_lep, label = year),
        hjust = 0, nudge_x = 0.05, size = 3.0) +
    geom_line(data = predicted_line,
        aes(x = mean_all, y = prop_lep),
        linetype = "dashed", colour = "grey50") + 
    scale_fill_manual(" ", values = c("purple1", "goldenrod2")) + 
    theme_bw() + 
    theme_mod_comp() +
    labs(x = "Mean number of lice per fish (all louse species)", 
        y = "Proportion of L. salmonis")
ggsave(filename = here::here("./figs/predicted-lep-proportions.png"),
        plot = predicted_proportions,
        width = 6,
        height = 5,
        dpi = 600)

### BEGIN NOTE ########
# So now that we know the proportion of all the years that we should expect to 
# be leps vs. caligus, we can go through each row and draw (using our known) 
# proportions of leps whether or not some unidentified stage should be a lep or 
# not. Those are all summed and then added later on.  
### END NOTE ########

prop_leps = pred_data_all_points %>% 
    dplyr::select(year, prop_lep)

# join the prop leps to the scfs_data so the proportion is in each row 
scfs_data = dplyr::left_join(
    scfs_data, 
    prop_leps, 
    by = "year"
)

set.seed(1234) 
# make a new column with new leps vs new cals 
scfs_data$new_lep = 0
scfs_data$new_cal = 0

# get a vector of the columns we want to address 
unid_lice = c("chala", "chalb", "unid_cope", "chal_unid", "unid_adult")

# crapy loop to draw the probability and fill in the value
for (row in seq_len(nrow(scfs_data))) { # go through each row 
    # go through the columns that aren't to species 
    for (col in unid_lice) { # go through each column of interest
        if (is.na(scfs_data[row, col]) | scfs_data[row, col] < 1)
            next # cut out NA's and zeros 
        # draw for each louse in that column
        for (louse in 1:data.frame(scfs_data[row, col])[1,1]) {
            draw = sample(c(1, 0), # sample between a one or a zero
                        size = 1, 
                        # the probability is the proportion of leps or 
                        # 1 minus that proportion for the caligus
                        prob = c(scfs_data[row, "prop_lep"], 
                            (1 - scfs_data[row, "prop_lep"])))
            ifelse (draw == 1, # condition
                # if the draw is 1 that one goes to the leps
                scfs_data[row, "new_lep"] <- scfs_data[row, "new_lep"] + 1,
                # if the draw is 0 that one goes to cals 
                scfs_data[row, "new_cal"] <- scfs_data[row, "new_cal"] + 1
                )
            }
        } 
    if(row %% 1000 == 0) {
        print(paste0("iteration: ", row, " -- "))
    }
}

# check what the maximum value should be 
unid_check = scfs_data %>% 
    dplyr::rowwise() %>% 
    dplyr::summarize(check = 
        sum(chala, chalb, unid_cope, chal_unid, unid_adult, na.rm = TRUE))
hist(unid_check$check)
### BEGIN NOTE ########
# This shows there are a couple observations where the sum is huge so then the 
# max of the new_lep column can be quite large  
### END NOTE ########

scfs_data_chal_inc = scfs_data %>% 
    dplyr::rowwise() %>%
    dplyr::mutate(all_lep = sum(lep_cope, lep_pamale, lep_pafemale, lep_male, 
                            lep_nongravid, lep_gravid,
                            # vars that got the correction factor included here
                            new_lep,
                            na.rm = TRUE),
                    all_cal = sum(cal_cope, cal_mot, cal_gravid,
                            # vars below here all need the correction factor
                            new_cal,
                            na.rm = TRUE), 
                    all_lice = sum(lep_cope, chala, chalb, lep_pamale, 
                                lep_pafemale, lep_male, lep_nongravid, 
                                lep_gravid, cal_cope, cal_mot, cal_gravid, 
                                unid_cope, chal_unid, unid_pa, unid_adult,
                                na.rm = TRUE),
                    date = lubridate::make_date(year, month, day))
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
                                na.rm = TRUE),
                    date = lubridate::make_date(year, month, day))
# set up both data with only the information we want & write it out ============

# scfs data
scfs_regress_chal_inc = data.frame(scfs_data_chal_inc %>% 
    dplyr::select(all_lice, all_lep, all_cal, 
                    month, year, day, date, farm)) %>% 
    # rename farm to farm_name to stay consistent
    dplyr::rename(
        farm_name = farm
    )

# need to make a column for week 
scfs_regress_chal_inc$week = 
    lubridate::week(lubridate::ymd(scfs_regress_chal_inc$date))

table(scfs_regress_chal_inc$week)

# exclude data from weeks 28, 33, 9 
scfs_regress_chal_inc = scfs_regress_chal_inc %>% 
    dplyr::filter(week %notin% c(28, 33, 9))

# write data
readr::write_csv(scfs_regress_chal_inc,
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))











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



