########## 
##########
# All functions for fitting frequentist models to the count data
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-24
##########
##########

options(dplyr.summarise.inform = FALSE)

library(here)
library(tidyverse)

farm_df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
all_scen_lice = read_csv(here("./data/wild-lice-data/clean/all-scenario-yearly-lice-per-fish-estimates.csv"))


make_farm_groupings = function(farm_df) {
  
  #' Make three groupings of the farms - all the farms, the KTC farms, 
  #' and the HSD Triangle Farms, return all three of these dataframes 
  #' grouped by years 
  
  all_farms = farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(
      all_leps = mean(lep_tot, na.rm = TRUE)
    )
  
  # make ktc farm df
  ktc_farms = farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(
      all_leps = mean(lep_tot, na.rm = TRUE)
    )
}