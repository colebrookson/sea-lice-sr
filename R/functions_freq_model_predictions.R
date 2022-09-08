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
scfs_df = read_csv(here("./data/prepped-data/scfs-regression-scen1-indiv.csv"))
model = readRDS(here("./outputs/model-outputs/lice-per-fish-regression/scenario-1-indiv/best-mod-negative-bionomial-model.rds"))
scenario = "scen3"

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

#############################
# prepare_scfs_data() function
#############################
prepare_scfs_data = function(scfs_df) {
  
  #' Take in the data and group by year then get a yearly mean average
  
  yearly_scfs_df = scfs_df %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(mean_lep = mean(all_lep))
  
  return(yearly_scfs_df)
}

#############################
# make_model_predictions() function
#############################
make_model_predictions = function(model, scenario) {
  
  #' Make a model prediction with the models in hand for each yearly level
  
  # make the prediction data
  predict_data = data.frame(
    year = as.character(c(2001:2021)),
    week = NA,
    farm_name = NA
  )
  
  # get the actual prediction and put it in a dataframe
  all_lep_df = data.frame(
    stats::predict(
      model,
      newdata = predict_data,
      type = "response",
      re.form = NA,
      se.fit = TRUE
    )
  )
  
  # now add in the values to the predict_data df
  predict_data = predict_data %>% 
    dplyr::mutate(
      fit = all_lep_df$fit,
      lower = all_lep_df$fit - (1.96 * all_lep_df$se.fit),
      upper = all_lep_df$fit + (1.96 * all_lep_df$se.fit),
      # add in which scenario this is
      scenario = scenario
    )
  
  return(predict_data)
}

#############################
# make_model_predictions() function
#############################
save_predict_data = function(df, path, scenario) {
  
  #' save the predicted data 
  
  readr::write_csv(df, paste0(path, scenario, ".csv"))
}

#############################
# execute_model_predictions() function
#############################
execute_model_predictions = function(scfs_df, farm_df, model, scenario, path) {
  
  #' Use the helper functions at hand to take in a fit model and put out a 
  #' prediction of that model across the viable values 
  
  # make sure both datasets are in the right form
  scfs_df = check_scfs_data_form(scfs_df)
  farm_df = check_farm_data_form(farm_df)
  
  # get a yearly version 
  yearly_scfs_df = prepare_scfs_data(scfs_df)
  
  # make the actual predictions
  predictions = make_model_predictions(model, scenario)
  
  # save the output 
  save_predict_data(predictions, path, scenario)
  
  # return the predictions df
  return(predictions)
  
}
