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
  
  df = df %>% 
    dplyr::mutate(
      area = as.factor(area),
      year_fac = as.factor(year),
      population_name = as.factor(population_name)
    ) %>% 
    dplyr::filter(
      !is.na(lice)
    )
}

#############################
# run_models() function 
#############################
run_models = function(df, 
                      #output_path
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
# perform_bootstrapping() function 
#############################
perform_bootstrapping = function(df, alt_model) {
  
  #' Do the actual bootstrapping 
  
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
  
  for(i in 2:n_jobs) job_seeds[i,] = parallel::nextRNGStream(job_seeds[i-1,])
  
  t0 = proc.time()
  cl = parallel::makeCluster(cores)
  parallel::clusterExport(cl, varlist = list("job_seeds", "sr_df", "parameters"))
  output = parallel::clusterApply(cl, x = c(1:n_jobs), fun = bootstrap)
}


library(tidyverse)
library(here)
library(lme4)
library(parallel)
library(broom.mixed)

df = read_csv(here("./data/prepped-data/stock-recruit-data-frames/stock-recruit-data-lice-included-3-pairs.csv"))

df = check_data(df)
alt_model = run_models(df)
df_rand_fixed = prepare_for_bootstrapping(df, alt_model)
sr_df = check_data(df_rand_fixed)


sr_df = readr::read_csv(here::here(
  "./data/prepped-data/stock-recruit-data-cut-off-03.csv"
))
sr_df[which(sr_df$area == 12 & sr_df$year %in% c(1991:2001)), "lice"] = NA
sr_df = subset(sr_df, is.na(lice)==FALSE)
#sr_df = sr_df[which(sr_df$year > 1961),]

sr_df$area = as.factor(sr_df$area)
sr_df$year_fac = as.factor(sr_df$year)
sr_df$population_name = as.factor(sr_df$population_name)