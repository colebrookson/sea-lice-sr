########## 
##########
# Get NuSEDs data in the right form
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

library(tidyverse)
library(here)
library(lme4)
library(parallel)
library(broom.mixed)

df = read_csv(here("./data/prepped-data/stock-recruit-data-frames/stock-recruit-data-lice-included-3-pairs.csv"))

df = check_data(df)
alt_model = run_models(df)
#############################
# check_data() function 
#############################
check_data = function(df) {
  
  #' make sure the columns that need to be factors are factors 
  
  df = df %>% 
    dplyr::mutate(
      area = as.factor(area),
      year_fac = as.factor(year),
      population_name = as.factor(population_name)
    )
}

#############################
# run_models() function 
#############################
run_models = function(df, 
                      output_path
                      ) {
  
  #' Run both the null model and the alternative model 
  
  # NULL MODEL
  null_model = lme4::lmer(log_survival ~ S:population_name + 
                            (1|year_fac/area),
                          data = df)
  # saveRDS(null_model, paste0(output_path, 
  #                            "null-model", 
  #                            unique(df$scenario),
  #                            "-",
  #                            unique(df$min_pop),
  #                            "-pairs.rds"))
  # extract vals of use
  coefs = broom::tidy(null_model)
  fitted_vals = broom::augment(null_model)
  model_vals = broom::glance(null_model)
  
  # readr::write_csv(coefs, 
  #                  paste0(output_path, "coefs-null-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  # readr::write_csv(fitted_vals, 
  #                  paste0(output_path, "fitted-vals-null-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  # readr::write_csv(model_vals, 
  #                  paste0(output_path, "model-vals-null-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  
  # ALTERNATIVE MODEL
  alt_model = lme4::lmer(log_survival ~ S:population_name +
                           lice + (1|year_fac/area),
                         data = df)
  # saveRDS(alt_model, paste0(output_path, 
  #                            "alt-model", 
  #                            unique(df$scenario),
  #                            "-",
  #                            unique(df$min_pop),
  #                            "-pairs.rds"))
  coefs = broom::tidy(alt_model)
  fitted_vals = broom::augment(alt_model)
  model_vals = broom::glance(alt_model)
  
  # readr::write_csv(coefs, 
  #                  paste0(output_path, "coefs-alt-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  # readr::write_csv(fitted_vals, 
  #                  paste0(output_path, "fitted-vals-alt-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  # readr::write_csv(model_vals, 
  #                  paste0(output_path, "model-vals-alt-model", 
  #                         unique(df$scenario), "-", 
  #                         unique(df$min_pop), "-pairs.csv"))
  
  # get the useful summary 
  print_info = 
    paste0("with lmer(), value of fitted r (growth rate) is ", 
      lme4::fixef(alt_model)[1],
      " and the value of c (effect of sea lice) is ", lme4::fixef(alt_model)[2])
  # readr::write_lines(
  #   print_info,
  #   paste0(output_path,
  #          "info-alt-model", unique(df$scenario), "-", 
  #          unique(df$min_pop), "-pairs.txt")
  # )
  
  return(alt_model)
}

#############################
# prepare_for_bootstrapping() function 
#############################
prepare_for_bootstrapping = function(df, alt_model) {
  
  #' Get the dataframe at hand ready to perform bootstrapping on 
  
  df_prepped = df %>% 
    dplyr::mutate(
      year_area = 0,
      r = 0
    )
  
  # extract random effects and fixed effects from model object 
  rand_effects = rownames(lme4::ranef(alt_model)$`area:year_fac`)
  fixed_effects = 
    names(lme4::fixef(alt_model))[3:length(lme4::fixef(alt_model))]
}




