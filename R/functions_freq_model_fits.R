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

library(tidyverse)
library(here)
library(glmmTMB)
library(DHARMa)
farm_df = read_csv(
  here::here("./data/farm-data/clean/all-farms-joined-clean.csv"))

scfs_df = read_csv(
  here::here(
    "./data/prepped-data/scfs-regression-scen3.csv"
  )
)

check_data_form = function(scfs_df, farm_df) {
  
  #' Check that the form of the important columns are in the type they need to 
  #' be in 
  
  scfs_df$all_lep = as.integer(scfs_df$all_lep)
  scfs_df$year = as.factor(as.character(scfs_df$year))
  scfs_farm
  
  
}
