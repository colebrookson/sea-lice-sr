########## 
##########
# All functions related to performing regressions on the wild lice data
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-01
##########
##########

options(dplyr.summarise.inform = FALSE)

# set up functions =============================================================

#############################
# read_data_clean_scfs() function
#############################
read_data_clean_scfs = function(file) {
  
  #' read in clean file to do analysis on 
  
  readr::read_csv(file)
  
}

#############################
# prep_data_mot() function
#############################
prep_data_mot = function(df) {
  
  #' Take in raw dataframe and cut it down to size, making two dataframes, one
  #' with data from 2001 onwards and one with only 2001 for comparison
  
  df_subset = df %>% 
    dplyr::select(year, prop_lep_mot, all_mot, lep_mot)
  
  df_2002_onwards = df_subset %>% 
    dplyr::filter(year > 2001)
  
  df_2001 = df_subset %>% 
    dplyr::filter(year == 2001)
  
  # list up return value 
  return_list = list(df_2001, df_2002_onwards)
  
  return(return_list)
}

#############################
# prep_data_cope() function
#############################
prep_data_cope = function(df) {
  
  #' Prep data on copepeodites to model 
  
  df_subset = df %>% 
    dplyr::select(year, prop_lep_cope, all_cope)
  
  df_2005_onwards = df_subset %>% 
    dplyr::filter(year > 2004)
  
  df_2002_2004 = df_subset %>% 
    dplyr::filter(year > 2001 & year < 2005)
  
  # list up the return df's
  return_list = list(df_2002_2004, df_2005_onwards)
}

#############################
# motile_regression() function
#############################
motile_regression = function(df) {
  
  #' Perform logistic regression on motile data, using the proportion as our 
  #' response variable -- here we are regressing the proportion of motile leps 
  #' against the number of all MOTILES, not just all lice. 
  
  mot_reg = stats::glm(prop_lep_mot ~ all_mot,
                       # binomial family
                       family = binomial(link = "logit"),
                       data = df)
  
  # save object as nice neat file 
  coefs = broom::tidy(mot_reg)
  fitted_vals = broom::augment(mot_reg)
  model_vals = broom::glance(mot_reg)
  
  # list results objects
  results_list = list(mot_reg, coefs, fitted_vals, model_vals)
  
  return(results_list)
  
}

#############################
# save_regression_mots() function
#############################
save_regression_mots = function(results_list, file) {
  
  #' take output of the multiple model objects and save them to a useful 
  #' location for further inspection if needed. The model object is saved as a
  #' `.rds` file, the rest are saved as text files
  
  # save model object
  saveRDS(
    results_list[[1]], 
    here::here(paste0(file, "mot_regress_model_ob.rds"))
  )
  # save coefs
  readr::write_csv(
    results_list[[2]], 
    here::here(paste0(file, "mot_regression_coefs.csv"))
  )
  # save fitted values 
  readr::write_csv(
    results_list[[3]], 
    here::here(paste0(file, "mot_regression_fitted_vals.csv"))
  )
  # save glance object
  readr::write_csv(
    results_list[[4]],
    here::here(paste0(file, "mot_regression_glance_aic.csv"))
  )
}

read_regression_mot = function(file) {
  
  #' Read in the fitted model object to make predictions on the dataframe from 
  
  readRDS(file)
}

#############################
# predicted_values_mots() function
#############################
predicted_values_mots = function(model, df) {
  
  #' Take in the fitted model object and make predictions for the values that 
  #' the model actually covers out to the maximum count of number of motiles
  
  # find the maximum number of motiles
  max_mot = max(df$all_mot)
  
  # make sequence of values ot predict on
  mot_seq = data.frame(all_mot = seq(0, max_mot, 0.01))
  
  # prediction for motiles
  pred_mot = data.frame(
    # all motiles count
    all_mots = mot_seq$all_mot,
    pred_prop = stats::predict(
      # model object here
      model, 
      mot_seq,
      type = "response",
      se.fit = TRUE
    )
  )
  
  # add 95% CI's
  pred_mot_ci = pred_mot %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      # fitted value
      pred_prop = pred_prop.fit,
      # lower CI bound
      lower = pred_prop - 1.96 * pred_prop.se.fit,
      # upper CI bound
      upper = pred_prop + 1.96 * pred_prop.se.fit) %>% 
    # keep only columns of use
    dplyr::select(all_mots, pred_prop, lower, upper)
  
  return(pred_mot_ci)
}

prediction_plot_mot = function(df, pred_df) {
  
  #' Make and save the prediction plot from the motiles regression 
  
  mot_plot = ggplot2::ggplot() + 
    geom_point(data = df, aes(x = all_mot, y = prop_lep_mot), 
               shape = 21,
               colour = "black",
               fill = "red2",
               alpha = 0.1,
               position = position_jitter()) +
    geom_ribbon(data = pred_df, aes(x = all_mots, ymin = lower, ymax = upper),
                fill = "grey80") +
    geom_line(data = pred_df, aes(x = all_mots, y = pred_prop)) + 
    theme_base() + 
    labs(x = "Number of All Motiles", y = "Proportion of L. salmonis") +
    scale_size_manual(values = c(3, 1))
  
  ggplot2::ggsave(
    mot_plot,
    here::here(".figs/mot-regression/motile-model-predictions.png")
  )
  
}
  
### testing
library(tidyverse)
library(here)
library(ggthemes)

raw_df = read.csv(here("./data/wild-lice-data/clean/scfs-data-clean.csv"))
df = raw_df
df = df_2002_onwards

