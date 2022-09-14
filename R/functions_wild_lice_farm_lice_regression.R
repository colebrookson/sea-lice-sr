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
    ) %>% 
    dplyr::mutate(
      log_all_leps = log(all_leps)
    )
  
  # make ktc farm df
  ktc_farms = farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(ktc == 1) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(
      ktc_leps = mean(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      log_ktc_leps = log(ktc_leps)
    )
  
  # make the hsd farms df 
  hsd_farms = farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(hump_sarg_doc == 1) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(
      hsd_leps = mean(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      log_hds_leps = log(hsd_leps)
    )
  
  # bind together all farm combos
  all_group_farms = cbind(
    all_farms,
    ktc_farms %>% 
      dplyr::select(-year),
    hsd_farms %>% 
      dplyr::select(-year)
  )
  
  return(all_group_farms)
  
}

reshape_scenario_lice = function(all_scen_lice) {
  
  #' Take the values and put them in wide format for easier regression
  
  tidyr::pivot_wider(
    all_scen_lice %>% 
      dplyr::select(-c(farm_name, week)) %>% 
      dplyr::mutate(scenario = as.factor(scenario)),
    names_from = scenario,
    values_from = fit
  )
}




all_group_farms = make_farm_groupings(farm_df)


