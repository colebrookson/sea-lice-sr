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

#############################
# check_data_form() function
#############################
check_data_form = function(scfs_df) {
  
  #' Check that the form of the important columns are in the type they need to 
  #' be in 
  
  scfs_df$all_lep = as.integer(scfs_df$all_lep)
  scfs_df$year = as.factor(as.character(scfs_df$year))
  scfs_df$farm_name = as.factor(scfs_df$farm_name)
  scfs_df$week = as.factor(scfs_df$week)
  
  return(scfs_df)
  
}

#############################
# get_n_cores() function
#############################
get_n_cores = function() {
  
  #' Simple function to return the number of cores present 
  
  # always leave one free 
  n_cores = min(parallel:: detectCores(), 8) - 1
  
  return(n_cores)

}

#############################
# nb_poi_zinb_zip() function
#############################
nb_poi_zinb_zip = function(df, n_cores, loc_path) {
  
  #' Take in the dataframe with the variable at hand and, using AIC, determine
  #' whether or not a standard or zero-inflated negative-binomial or poisson
  #' model fits better to the data
  
  # negative binomial function
  nb_mod = glmmTMB::glmmTMB(
    all_lep ~ year + (1 | week) + (1 | farm_name),
    data = df,
    family = nbinom2,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS"),
      parallel = n_cores
    )
  )
  # save the model object
  saveRDS(nb_mod, paste0(loc_path, "negative-bionomial-model.rds"))
  
  # zero-inflated negative binomial
  zinb_mod = glmmTMB::glmmTMB(
    all_lep ~ year + (1 | week) + (1 | farm_name),
    data = df,
    family = nbinom2,
    ziformula = ~0,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS"),
      parallel = n_cores
    )
  )
  # save the model object
  saveRDS(zinb_mod, paste0(loc_path, 
                           "zero-inflated-negative-bionomial-model.rds"))
  
  # poisson model
  poi_mod = glmmTMB::glmmTMB(
    all_lep ~ year + (1 | week) + (1 | farm_name),
    data = df,
    family = poisson,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS"),
      parallel = n_cores
    )
  )
  # save the model object
  saveRDS(poi_mod, paste0(loc_path, "poisson-model.rds"))
  
  # zero-inflated poisson model 
  zip_mod = glmmTMB::glmmTMB(
    all_lep ~ year + (1 | week) + (1 | farm_name),
    data = df,
    family = poisson,
    ziformula = ~0,
    control = glmmTMBControl(
      optimizer = optim,
      optArgs = list(method = "BFGS"),
      parallel = n_cores
    )
  )
  # save the model object
  saveRDS(zinb_mod, paste0(loc_path, "zero-inflatedpoisson-model.rds"))
  
  # find the best model (lowest AIC)
  mod_list = list(nb_mod = nb_mod, zinb_mod = zinb_mod, 
              poi_mod = poi_mod, zip_mod = zip_mod)
  aic_list = c(nb_mod = AIC(nb_mod), zinb_mod = AIC(zinb_mod), 
               poi_mod = AIC(poi_mod), zip_mod = AIC(zip_mod))
  
  best_mod = aic_list[which(aic_list == min(aic_list))]
  
  # extract best model object
  best_mod_ob = mod_list[which(names(mod_list) == names(best_mod))][[1]]

  if(names(best_mod) == "nb_mod") {
    best_mod_name = "Negative Binomial"
  } else if(names(best_mod) == "zinb_mod") {
    best_mod_name = "Zero-Inflated Negative Binomial"
  } else if(names(best_mod) == "poi_mod") {
    best_mod_name = "Poisson"
  } else if(names(best_mod) == "zip_mod") {
    best_mod_name = "Zero-Inflated Poisson"
  }
  
  return(best_mod_ob, best_mod_name)
}

#############################
# model_resids() function
#############################
model_resids = function(best_mod_ob, path) {
  
  #' Take in the best model object and simulate the residuals 
  
  # extract residuals
  res = simulateResiduals(best_mod_ob)

  # save the residuals
  png(filename = paste0(path, "best-model-residuals.png"))
  
  # plot the residuals
  plot(res)
  
  dev.off()
  
}













df = check_data_form(scfs_df)

n_cores = get_n_cores()
