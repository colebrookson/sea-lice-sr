########## 
##########
# Get NuSEDs data in the right form
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########


#############################
# check_data() function 
#############################
check_data = function(df) {
  
  #' make sure the columns that need to be factors are factors 
  
  df_fixed = df %>% 
    dplyr::mutate(
      area = as.factor(area),
      year_fac = as.factor(year),
      population_name = as.factor(population_name)
    ) %>% 
    # really important to get rid of the NA's or the bootstrapping
    # won't work
    dplyr::filter(
      !is.na(lice)
    )
  
  return(df_fixed)
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
  saveRDS(null_model, paste0(output_path,
                             "null-model",
                             unique(df$scenario),
                             "-",
                             unique(df$min_pop),
                             "-pairs.rds"))
  # extract vals of use
  coefs = broom::tidy(null_model)
  fitted_vals = broom::augment(null_model)
  model_vals = broom::glance(null_model)
  
  readr::write_csv(coefs,
                   paste0(output_path, "coefs-null-model",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))
  readr::write_csv(fitted_vals,
                   paste0(output_path, "fitted-vals-null-model",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))
  readr::write_csv(model_vals,
                   paste0(output_path, "model-vals-null-model",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))
  
  # ALTERNATIVE MODEL
  alt_model = lme4::lmer(log_survival ~ S:population_name +
                           lice + (1|year_fac/area),
                         data = df)
  saveRDS(alt_model, paste0(output_path,
                             "alt-model",
                             unique(df$scenario),
                             "-",
                             unique(df$min_pop),
                             "-pairs.rds"))
  coefs = broom::tidy(alt_model)
  fitted_vals = broom::augment(alt_model)
  model_vals = broom::glance(alt_model)
  
  readr::write_csv(coefs,
                   paste0(output_path, "coefs-alt-model-",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))
  readr::write_csv(fitted_vals,
                   paste0(output_path, "fitted-vals-alt-model-",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))
  readr::write_csv(model_vals,
                   paste0(output_path, "model-vals-alt-model-",
                          unique(df$scenario), "-",
                          unique(df$min_pop), "-pairs.csv"))

  # get the useful summary 
  print_info = 
    paste0("with lmer(), value of fitted r (growth rate) is ", 
      lme4::fixef(alt_model)[1],
      " and the value of c (effect of sea lice) is ", lme4::fixef(alt_model)[2])
  readr::write_lines(
    print_info,
    paste0(output_path,
           "info-alt-model-", unique(df$scenario), "-",
           unique(df$min_pop), "-pairs.txt")
  )
  
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
  rand_effects = data.frame(
    rand = rownames(lme4::ranef(alt_model)$`area:year_fac`)
  ) %>% 
    tidyr::separate(rand, c("area", "year"), ":") %>% 
    dplyr::mutate(
      # make new column that gives a number to each area/year combo
      year_area = as.numeric(c(1:nrow(.)))
    )
  
  fixed_effects = data.frame(
    fixed_effects = names(lme4::fixef(alt_model))[3:length(lme4::fixef(alt_model))]
  ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      population_name = stringr::str_remove(
        fixed_effects, "S:population_name"
      )
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      # make new column that gives a number to each area/year combo
      r = as.numeric(c(1:nrow(.)))
    ) %>% 
    dplyr::select(-fixed_effects)
  
  # join these dataframes to the focal dataframe to get the r and area_year
  # values to the df 
  df_rand_fixed = dplyr::left_join(
    df, 
    rand_effects, 
    by = c("area", "year_fac" = "year")
  ) %>% 
    dplyr::left_join(
      .,
      fixed_effects,
      by = "population_name"
    ) %>% 
    dplyr::select(
      -c(scenario, min_pop)
    )
  
  return(df_rand_fixed)
}

#############################
# bootstrap() function 
#############################
bootstrap = function(x) {
  
  # set the values that we want 
  a = parameters[[1]]; b_i = parameters[[2]]; c = parameters[[3]]; 
  sigma_ya = parameters[[4]]; sigma_y = parameters[[5]]
  sigma_e = parameters[[6]]
  
  # set a seed 
  set.seed(job_seeds[x,2])
  
  R = numeric(length(sr_df$S)) # this will be simulated survival
  theta_y = rnorm(length(levels(sr_df$year_fac)), 0, sigma_y)
  theta_ya = rnorm(max(sr_df$year_area), 0, sigma_ya)
  epsilon = rnorm(length(sr_df$S), 0, sigma_e)
  
  # calculate R 
  R = sr_df$S * exp(
    a + 
      b_i[sr_df$r] * sr_df$S + 
      c * sr_df$lice + 
      theta_ya[sr_df$year_area] + 
      theta_y[as.numeric(sr_df$year_fac)] +
      epsilon)
  
  # get survival 
  SS = log(R/sr_df$S)
  
  # make a temporary dataframe 
  temp_sr_df = data.frame(
    area = as.factor(sr_df$area),
    population_name = as.factor(sr_df$population_name),
    year_fac = as.factor(sr_df$year_fac),
    S = sr_df$S,
    survival = SS,
    lice = sr_df$lice
  )
  
  # actually fit the model
  model = lme4::lmer(survival ~ S:population_name + 
                       lice + (1|year_fac/area),
                     data = temp_sr_df, REML = TRUE)
  
  # get the results 
  params_result = lme4::fixef(model)[1:2]
  return(params_result)
}

#############################
# prep_bootstrap_data() function 
#############################
prep_bootstrap_data = function(df, output_path) {
  
  #' Use helper functions to make the actual estimate of the c value 
  
  # make sure all formats of the data are correct 
  df = check_data(df)
  
  # fit the model and null model 
  alt_model = run_models(df, output_path)
   
  # # get the values we need for bootstrapping to occur)
  df_rand_fixed = prepare_for_bootstrapping(df, alt_model)
  
  # # check the data again before it actually goes 
  sr_df = check_data(df_rand_fixed)
  
  # list up the results
  results_list = list(df, alt_model, df_rand_fixed, sr_df)
   
  return(results_list)
  
}


#############################
# perform_bootstrapping() function 
#############################
perform_bootstrapping = function(df, output_path) {
  
  #' Do the actual bootstrapping 
  
  df = check_data(df)
  
  # fit the model and null model 
  alt_model = run_models(df, output_path)
  
  # # get the values we need for bootstrapping to occur)
  df_rand_fixed = prepare_for_bootstrapping(df, alt_model)
  
  # # check the data again before it actually goes 
  sr_df = check_data(df_rand_fixed)
  # unlist results
  # df = results_list[[1]]
  # alt_model = results_list[[2]]
  # df_rand_fixed = results_list[[3]]
  # sr_df = results_list[[4]]
  
  # set up estimated variances for bootstrap algorithm
  b = as.numeric(lme4::fixef(alt_model)[3:length(lme4::fixef(alt_model))])
  a = as.numeric(lme4::fixef(alt_model)[1]) # intercept 
  c = as.numeric(lme4::fixef(alt_model)[2]) # lice
  
  # get the standard deviations 
  sigma_ya = attr(lme4::VarCorr(alt_model)$`area:year_fac`, "stddev")[[1]]
  sigma_y = attr(lme4::VarCorr(alt_model)$`year`, "stddev")[[1]]
  sigma_e = attr(lme4::VarCorr(alt_model), "sc")
  
  parameters = list(a, b, c, sigma_ya, sigma_y, sigma_e)
  
  # run this all in parallel 
  cores = detectCores()-1 # keep one for processing other things 
  
  # get random sequences for different chains 
  n_jobs = 1000
  RNGkind("L'Ecuyer-CMRG")
  set.seed(1234)
  job_seeds = matrix(nrow = n_jobs, ncol = 7)
  job_seeds[1,] = .Random.seed
  
  for(i in 2:n_jobs){
    job_seeds[i,] = parallel::nextRNGStream(job_seeds[i-1,])
  } 
  
  t0 = proc.time()
  cl = parallel::makeCluster(cores)
  parallel::clusterExport(cl, varlist = list("job_seeds",
                                             "sr_df", "parameters"),
                          envir = environment())
  output = parallel::clusterApply(cl, x = c(1:n_jobs), fun = bootstrap)
  # parallel::stopCluster(cl)
  saveRDS(output, paste0(output_path,
                         "bootstrap-output",
                         unique(df$scenario),
                         "-",
                         unique(df$min_pop),
                         "-min-pops",
                         ".rds"))

  # unlist results
  p_all = matrix(nrow = n_jobs, ncol = 2)
  for(i in 1:n_jobs) p_all[i,] = as.numeric(output[[i]])

  # now make the actual confidence intervals
  ci = apply(p_all, 2, quantile, c(0.025, 0.975))
  ci = rbind(ci[1,], as.numeric(
    lme4::fixef(alt_model)[1:2]), ci[2,])
  colnames(ci) = c("r", "c")
  ci = data.frame(ci) %>%
    dplyr::mutate(
      value = c("lower", "MLE", "upper")
    )

  readr::write_csv(
    ci,
    paste0(output_path,
           "estimate-of-c-",
           unique(df$scenario),
           "-",
           unique(df$min_pop),
           "min-pops",
           ".csv")
  )

  return(ci)
}

#############################
# execute_c_estimate() function 
#############################
#' execute_c_estimate = function(results_list, output_path) {
#'   
#'   #' Take in values and get estimates and confidence intervals
#'   #' for the values of the "c" parameter
#'   
#'   # unlist results
#'   df = results_list[[1]]
#'   alt_model = results_list[[2]]
#'   df_rand_fixed = results_list[[3]]
#'   sr_df = results_list[[4]]
#'   
#'   # perform the actual bootstrapping
#'   ci = perform_bootstrapping(sr_df, alt_model, output_path, df)
#'   
#'   return(ci)
#' }

#############################
# get_percent_mortality_estimates() function 
#############################
get_percent_mortality_estimates = function(results_list, ci, output_path) {
  
  #' use our formula for mortality to extract the lower, upper, and lme
  #' values for each year we have in the model 
  
  # unlist results
  df = results_list[[1]]
  alt_model = results_list[[2]]
  df_rand_fixed = results_list[[3]]
  sr_df = results_list[[4]]
  
  # make a df for the louse values 
  lice_df = unique(sr_df[which(sr_df$area == 12 & 
                                 sr_df$year > 2001), c("lice", "year")])
  
  # get mortality extimates
  mortality = data.frame(
    mle = ci$c[which(ci$value=="MLE")]*lice_df$lice, # mle
    upper = ci$c[which(ci$value=="upper")]*lice_df$lice, # 2.5%
    lower = ci$c[which(ci$value=="lower")]*lice_df$lice # 97.5%
  ) 
  # percentage mortality 
  p_mort = 100*(1-exp(mortality))
  colnames(p_mort) = c("MLE", "lower", "upper")
  p_mort = p_mort %>% 
    dplyr::mutate(
      year = c(2002:2016)
    )
  
  readr::write_csv(
    p_mort,
    paste0(output_path,
           "mortality-estimates-",
           unique(df$scenario),
           "-",
           unique(df$min_pop),
           "min-pops",
           ".csv")
  )
  
  return(p_mort)
}

#############################
# predict_future_mortality() function 
#############################
predict_future_mortality = function(results_list, p_mort, 
                                    predict_df, output_path) {
  
  #' Since we determine the survival is directly equal ot 1 - exp(-cWa, t-1)
  #' we can predict the survival values for 2017 to 2021
  
  # unlist results
  df = results_list[[1]]
  alt_model = results_list[[2]]
  df_rand_fixed = results_list[[3]]
  sr_df = results_list[[4]]
  
  # get just the scenario at hand 
  predict_df = predict_df %>% 
    dplyr::filter(scenario == unique(df$scenario)) %>% 
    dplyr::rename(all_lep = fit)
  
  add_years = data.frame(
    predict_df[which(predict_df$year >= 2016), "all_lep"],
    predict_df[which(predict_df$year >= 2016), "year"],
    predict_df[which(predict_df$year >= 2016), "upper"],
    predict_df[which(predict_df$year >= 2016), "lower"]
  )
  add_years$surv = NA
  add_years$surv_up = NA
  add_years$surv_low = NA
  
  # use established relationship to make prediction for further years
  for(i in 2:nrow(add_years)) {
    
    # use the MLE to get the estimated value
    add_years[i, "surv"] = 100*(1 - exp(
      -0.2346664 * add_years[i-1, "all_lep"])
    )
    # use the upper and lower bounds ofthe 95% CI to get the same for the est here
    add_years[i, "surv_up"] = 100*(1 - exp(
      -0.2346664 * add_years[i-1, "upper"])
    )
    add_years[i, "surv_low"] = 100*(1 - exp(
      -0.2346664 * add_years[i-1, "lower"])
    )
  }
  # get rid of extraneous years 
  add_years = add_years %>% 
      dplyr::select(surv, surv_up, surv_low, year) %>% 
      dplyr::filter(!is.na(surv)) %>% 
      dplyr::rename(MLE = surv, upper = surv_up, lower = surv_low)
  
  # put the two data pieces together and make a column to differentiate
  est_mort_df = rbind(
    p_mort,
    add_years
  )
  
  est_mort_df = est_mort_df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      predict = ifelse(year > 2016, "predicted", "estimated"),
      scenario = unique(df$scenario)
    )
  
  readr::write_csv(
    est_mort_df,
    paste0(output_path,
           "future-predicted-mortality-estimates-",
           unique(df$scenario),
           "-",
           unique(df$min_pop),
           "min-pops",
           ".csv")
  )
  
  return(est_mort_df)
}


# library(tidyverse)
# library(here)
# library(lme4)
# library(parallel)
# library(broom.mixed)
# 
# df = read_csv(here("./data/prepped-data/stock-recruit-data-frames/stock-recruit-data-lice-included-3-pairs.csv"))
# predict_df = read_csv(here("./data/wild-lice-data/clean/all-scenario-yearly-lice-per-fish-estimates.csv"))
# 
# results_list = prep_bootstrap_data(df, here::here(
#   "./outputs/model-outputs/stock-recruit-models/"
# ))
# ci = perform_bootstrapping(results_list,
#                         here::here(
#                           "./outputs/model-outputs/stock-recruit-models/"
#                         ))
# 
# 
# df = check_data(df)
# alt_model = run_models(df)
# df_rand_fixed = prepare_for_bootstrapping(df, alt_model)
# sr_df = check_data(df_rand_fixed)
# 
# 
# sr_df = readr::read_csv(here::here(
#   "./data/prepped-data/stock-recruit-data-cut-off-03.csv"
# ))
# sr_df[which(sr_df$area == 12 & sr_df$year %in% c(1991:2001)), "lice"] = NA
# sr_df = subset(sr_df, is.na(lice)==FALSE)
# #sr_df = sr_df[which(sr_df$year > 1961),]
# 
# sr_df$area = as.factor(sr_df$area)
# sr_df$year_fac = as.factor(sr_df$year)
# sr_df$population_name = as.factor(sr_df$population_name)