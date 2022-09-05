########## 
##########
# All functions for making predictions of yearly count data based on the 
# frequentist model fits
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-24
##########
##########

library(tidyverse)
library(here)
library(glmmTMB)
library(lme4)
library(PNWColors)
library(mgcv)
library(patchwork)

farm_df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
scfs_df = read_csv(here("./data/wild-lice-data/clean/chalimus-counted-lice.csv"))
model = readRDS(here("./outputs/model-outputs/lice-per-fish-regression/scenario-1-year/"))

#############################
# check_scfs_data_form() function
#############################
check_scfs_data_form = function(scfs_df) {
  
  #' Check that the form of the important columns are in the type they need to 
  #' be in 
  
  scfs_df$all_lep = as.integer(scfs_df$all_lep)
  scfs_df$year = as.factor(as.character(scfs_df$year))
  scfs_df$farm_name = as.factor(scfs_df$farm_name)
  scfs_df$week = as.factor(scfs_df$week)
  
  return(scfs_df)
  
}

#############################
# check_farm_data_form() function
#############################
check_farm_data_form = function(farm_df) {
  
  #' Check that the form of the important columns are in the type they need to 
  #' be in 
  
  farm_df$year = as.factor(as.character(farm_df$year))
  farm_df$farm_name = as.factor(farm_df$farm_name)

  return(farm_df)
  
}