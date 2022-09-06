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

#############################
# check_data_form() function
#############################
check_data_form = function(df) {
  
  #' Check that the form of the important columns are in the type they need to 
  #' be in 
  
  df$all_lep = as.integer(df$all_lep)
  df$year = as.factor(as.character(df$year))
  df$farm_name = as.factor(df$farm_name)
  df$week = as.factor(df$week)
  
  return(df)
  
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
df = read_csv(here("./data/prepped-data/scfs-regression-scen1-indiv.csv"))
n_cores = 3
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
  
 
  # find the best model (lowest AIC)
  mod_list = list(nb_mod = nb_mod, zinb_mod = zinb_mod, 
              poi_mod = poi_mod, zip_mod = zip_mod)
  aic_list = c(nb_mod = AIC(nb_mod), zinb_mod = AIC(zinb_mod), 
               poi_mod = AIC(poi_mod), zip_mod = AIC(zip_mod))
  
  best_mod = aic_list[which(aic_list == min(aic_list))]
  
  # extract best model object
  best_mod_ob = mod_list[which(names(mod_list) == names(best_mod))][[1]]
  # also save the best model
  saveRDS(best_mod_ob, paste0("best-mod-", names(best_mod), ".rds"))

  if(names(best_mod) == "nb_mod") {
    best_mod_name = "Negative Binomial"
    # save the model object
    saveRDS(nb_mod, paste0(loc_path, "best-mod-negative-bionomial-model.rds"))
  } else if(names(best_mod) == "zinb_mod") {
    best_mod_name = "Zero-Inflated Negative Binomial"
    # save the model object
    saveRDS(zinb_mod, 
            paste0(loc_path, 
                   "best-mod-zero-inflated-negative-binomial-model.rds"))
  } else if(names(best_mod) == "poi_mod") {
    best_mod_name = "Poisson"
    # save the model object
    saveRDS(poi_mod, paste0(loc_path, "best-mod-poisson-model.rds"))
  } else if(names(best_mod) == "zip_mod") {
    best_mod_name = "Zero-Inflated Poisson"
    # save the model object
    saveRDS(poi_mod, paste0(loc_path,
                            "best-mod-zero-inflated-poisson-model.rds"))
  }
  
  # list up the results
  results_list = list(best_mod_ob, best_mod_name)
  
  return(results_list)
}

#############################
# model_resids() function
#############################
model_resids = function(results_list, path) {
  
  #' Take in the best model object and simulate the residuals 
  
  # get just the best model object itself
  best_mod_ob = results_list[[1]]
  
  # extract residuals
  res = simulateResiduals(best_mod_ob)

  # save the residuals
  png(filename = paste0(path, "best-model-residuals.png"))
  
  # plot the residuals
  plot(res)
  
  dev.off()
  
}

#############################
# execute_scenario_models() function
#############################
execute_scenario_models = function(df, loc_path) {
  
  #' Use helper functions to get the best model for the given scenario
  
  # make sure the data are all in the right format
  df = check_data_form(df)
  
  # find the number of cores the models can run on
  n_cores = get_n_cores()
  
  # run the actual models 
  results_list = nb_poi_zinb_zip(df, n_cores, loc_path)
  
  # save residuals
  model_resids(results_list, loc_path)
  
  # return the object of the model fit
  return(results_list[[1]])
  
}
