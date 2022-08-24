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

# Motile stage functions =======================================================

#############################
# prep_data_mot() function
#############################
prep_data_mot = function(df) {
  
  #' Take in raw dataframe and cut it down to size, making two dataframes, one
  #' with data from 2001 onwards and one with only 2001 for comparison
  
  df_2002_onwards = df %>% 
    dplyr::filter(year > 2001) %>% 
    dplyr::filter(all_mot > 0)
  
  df_2001 = df %>% 
    dplyr::filter(year == 2001)
  
  # list up return value 
  return_list = list(df_2001, df_2002_onwards, df)
  
  return(return_list)
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

#############################
# prediction_plot_mot() function
#############################
prediction_plot_mot = function(df, pred_df) {
  
  #' Make and save the prediction plot from the motiles regression 
  
  mot_plot = 
    ggplot2::ggplot() + 
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
    here::here("./figs/mot-regression/motile-model-predictions.png"),
    mot_plot
  )
  
}

#############################
# mot_2001_prediction() function
#############################
mot_2001_prediction = function(df, model) {
  
  #' Use the model object to make the prediction for the year 2001 
  
  # make prediction for mots
  mot_2001_pred = cbind(
    df,
    pred_prop = stats::predict(
      model, 
      data.frame(all_mot = df$all_mot),
      type = "response")
  )
  
  return(mot_2001_pred)
}

#############################
# lep_regression_mot() function
#############################
lep_regression_mot = function(df, path) {
  
  #' Combine all L. salmonis Motile regression functions into one function for 
  #' an easy target
  
  # prepare the data
  clean_df_list = prep_data_mot(df) 
  
  df_2001 = clean_df_list[[1]]
  df_2002_onwards = clean_df_list[[2]]
  df_subset = clean_df_list[[3]]
  
  # run the regression 
  regression = motile_regression(df_2002_onwards)
  
  # save regression
  save_regression_mots(regression, path)
  
  # make predictions 
  pred_df = predicted_values_mots(regression[[1]], df_2002_onwards)
  
  # make prediction plot
  prediction_plot_mot(df_2002_onwards, pred_df)
  
  # make 2001 prediction
  mot_2001_pred = mot_2001_prediction(df_2001, regression[[1]])
  
  # bind all of these results objects into a list
  results_list = list(clean_df_list, regression, pred_df, mot_2001_pred)
  
  # save that list
  saveRDS(results_list, paste0(path, 
                               "motile_regression_full_analysis_object.rds"))
  
  return(results_list)
  
}

# Cope functions ===============================================================

#############################
# prep_data_cope() function
#############################
prep_data_cope = function(df) {
  
  #' Prep data on copepeodites to model 
  
  df_2005_onwards = df %>% 
    dplyr::filter(year > 2004) %>% 
    # make sure no zeros in the fitting data
    dplyr::filter(all_cope > 0)
  
  df_2002_2004 = df %>% 
    dplyr::filter(year > 2001 & year < 2005)
  
  df = df
  
  # list up the return df's
  return_list = list(df_2002_2004, df_2005_onwards, df) 
  
  return(return_list)
}

#############################
# cope_regression() function
#############################
cope_regression = function(df) {
  
  #' Perform logistic regression on motile data, using the proportion as our 
  #' response variable -- here we are regressing the proportion of motile leps 
  #' against the number of all MOTILES, not just all lice. 
  
  cope_reg = glm(prop_lep_cope ~ all_cope,
                 # binomial family
                 family = binomial(link = "logit"),
                 data = df)
  
  # save object as nice neat file 
  coefs = broom::tidy(cope_reg)
  fitted_vals = broom::augment(cope_reg)
  model_vals = broom::glance(cope_reg)
  
  # list results objects
  results_list = list(cope_reg, coefs, fitted_vals, model_vals)
  
  return(results_list)
  
}

#############################
# save_regression_copes() function
#############################
save_regression_copes = function(results_list, file) {
  
  #' take output of the multiple model objects and save them to a useful 
  #' location for further inspection if needed. The model object is saved as a
  #' `.rds` file, the rest are saved as text files
  
  # save model object
  saveRDS(
    results_list[[1]], 
    here::here(paste0(file, "cope_regress_model_ob.rds"))
  )
  # save coefs
  readr::write_csv(
    results_list[[2]], 
    here::here(paste0(file, "cope_regression_coefs.csv"))
  )
  # save fitted values 
  readr::write_csv(
    results_list[[3]], 
    here::here(paste0(file, "cope_regression_fitted_vals.csv"))
  )
  # save glance object
  readr::write_csv(
    results_list[[4]],
    here::here(paste0(file, "cope_regression_glance_aic.csv"))
  )
}

#############################
# predicted_values_copes() function
#############################
predicted_values_copes = function(model, df) {
  
  #' Take in the fitted model object and make predictions for the values that 
  #' the model actually covers out to the maximum count of number of copepodites
  
  # find the maximum number of copepodites
  max_cope = max(df$all_cope)
  
  # make sequence of values ot predict on
  cope_seq = data.frame(all_cope = seq(0, max_cope, 0.01))
  
  # prediction for copepodites
  pred_cope = data.frame(
    # all copepodites count
    all_cope = cope_seq$all_cope,
    pred_prop = stats::predict(
      # model object here
      model, 
      cope_seq,
      type = "response",
      se.fit = TRUE
    )
  )
  
  # add 95% CI's
  pred_cope_ci = pred_cope %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      # fitted value
      pred_prop = pred_prop.fit,
      # lower CI bound
      lower = pred_prop - 1.96 * pred_prop.se.fit,
      # upper CI bound
      upper = pred_prop + 1.96 * pred_prop.se.fit) %>% 
    # keep only columns of use
    dplyr::select(all_cope, pred_prop, lower, upper)
  
  return(pred_cope_ci)
}

#############################
# prediction_plot_cope() function
#############################
prediction_plot_cope = function(df, pred_df) {
  
  #' Make and save the prediction plot from the motiles regression 
  
  cope_plot = 
    ggplot2::ggplot() + 
    geom_point(data = df, aes(x = all_cope, y = prop_lep_cope), 
               shape = 21,
               colour = "black",
               fill = "blue2",
               alpha = 0.1,
               position = position_jitter()) +
    geom_ribbon(data = pred_df, aes(x = all_cope, ymin = lower, ymax = upper),
                fill = "grey80") +
    geom_line(data = pred_df, aes(x = all_cope, y = pred_prop)) + 
    theme_base() + 
    labs(x = "Number of All Copepodites", y = "Proportion of L. salmonis") +
    scale_size_manual(values = c(3, 1))
  
  ggplot2::ggsave(
    here::here("./figs/cope-regression/cope-model-predictions.png"),
    cope_plot
  )
}

#############################
# cope_2002_2004_prediction() function
#############################
cope_2002_2004_prediction = function(df, model) {
  
  #' Use the model object to make the prediction for the years 2002-2004
  
  # copes model prediction
  cope_2002_2004_pred = data.frame(
    df,
    # predicted column
    pred_prop = stats::predict(
      model,
      data.frame(all_cope = df$all_cope),
      type = "response")
  )
  
  return(cope_2002_2004_pred)
}

#############################
# lep_regression_cope() function
#############################
lep_regression_cope = function(df, path) {
  
  #' Combine all L. salmonis Motile regression functions into one function for 
  #' an easy target
  
  # prepare the data
  clean_df_list = prep_data_cope(df) 
  
  df_2002_2004 = clean_df_list[[1]]
  df_2005_onwards = clean_df_list[[2]]
  df_subset = clean_df_list[[3]]
  
  # run the regression 
  regression = cope_regression(df_2005_onwards)
  
  # save regression
  save_regression_copes(regression, path)
  
  # make predictions 
  pred_df = predicted_values_copes(regression[[1]], df_2005_onwards)
  
  # make prediction plot
  prediction_plot_cope(df_2005_onwards, pred_df)
  
  # make 2001 prediction
  cope_pred = cope_2002_2004_prediction(df_2002_2004, regression[[1]])
  
  # bind all of these results objects into a list
  results_list = list(clean_df_list, regression, pred_df, cope_pred)
  
  # save list of results
  saveRDS(results_list, paste0(path, 
                               "cope_regression_full_analysis_object.rds"))
  
  return(results_list)
  
}

# Nonlinear regression functions ===============================================

#############################
# prep_data_nonlinear() function
#############################
prep_data_nonlinear = function(df) {
  
  #' Use the raw dataset, and clean it to prepare for the non-linear regression
  
  df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate( # make columns that divide the lice into species 
      all_lep = sum(lep_pamale, lep_pafemale, lep_male, 
                    lep_nongravid, lep_gravid, unid_pa, 
                    na.rm = TRUE),
      all_lice = sum(lep_pamale, 
                     lep_pafemale, lep_male, lep_nongravid, 
                     lep_gravid, cal_mot, cal_gravid,
                     unid_adult, unid_pa, 
                     na.rm = TRUE)
      ) %>% 
      dplyr::select( # keep only the columns we want
      year, all_lep, all_lice
      ) %>%
    dplyr::mutate(year = as.factor(year)) %>% 
      dplyr::group_by(
        year
      ) %>% 
      dplyr::summarize( # find yearly means for the two we want
        mean_lep = mean(all_lep, na.rm = TRUE),
        mean_all = mean(all_lice, na.rm = TRUE)
      ) %>% 
      dplyr::filter( # keep this out since it's getting predicted
        year != 2001
      ) %>% 
      dplyr::mutate( # find the proportion of all the lice that are leps
        prop_lep = mean_lep / mean_all
      )
}

#############################
# nonlinear_regression() function
#############################
nonlinear_regression = function(df) {
  
  #' Take prepared dataset and fit the non-linear model from Bateman et al. 
  #' (2016) - CJFAS
  #' Note the model is a non-linear regression (asymptotic) 
  #' to get the shape of the curve. It is fit with Y = a - (a - b) * exp(-cX) 
  #' note that the value for a is fixed at 1.0 since it's an actual hard 
  #' asymptote
  
  model = stats::nls(
    formula = prop_lep ~ 1.0 - (1.0 - 0.0) * exp(- c * mean_all), 
    start = list(c = 2), 
    data = df)
  
  # save object as nice neat file 
  coefs = broom::tidy(model)
  fitted_vals = broom::augment(model)
  model_vals = broom::glance(model)
  
  # list results objects
  results_list = list(model, coefs, fitted_vals, model_vals)
  
  return(results_list)
}

#############################
# save_nonlinear_regression() function
#############################
save_nonlinear_regression = function(results_list, file) {
  
  #' take output of the multiple model objects and save them to a useful 
  #' location for further inspection if needed. The model object is saved as a
  #' `.rds` file, the rest are saved as text files
  
  # save model object
  saveRDS(
    results_list[[1]], 
    here::here(paste0(file, "nonlinear_regress_model_ob.rds"))
  )
  # save coefs
  readr::write_csv(
    results_list[[2]], 
    here::here(paste0(file, "nonlinear_regress_coefs.csv"))
  )
  # save fitted values 
  readr::write_csv(
    results_list[[3]], 
    here::here(paste0(file, "nonlinear_regress_fitted_vals.csv"))
  )
  # save glance object
  readr::write_csv(
    results_list[[4]],
    here::here(paste0(file, "nonlinear_regress_glance_aic.csv"))
  )
}

#############################
# nonlinear_prediction() function
#############################
nonlinear_prediction = function(df, model) {
  
  #' Use the model object to predict across new values of all lice to get an 
  #' estimate for higher numbers of total lice - thus to predict 2001
  
  # predict the data back from the model 
  pred_data_points = data.frame(
    mean_all = seq(0,4,0.01)
  )
  
  # make prediction
  pred_prop = stats::predict(
    model, 
    pred_data_points,
    type = "response")
  
  # predicted dataframe
  df_line = 
    data.frame(
      cbind(pred_data_points, pred_prop)
      ) %>% 
    dplyr::rename(
      prop_lep = pred_prop
      )
  
  return(df_line)
}

#############################
# nonlinear_plot() function
#############################
nonlinear_plot = function(df, df_line) {
  
  #' Make and save a plot of the non-linear prediction based off the prediction
  #' for 2001 in this framework
  
  # make data for a plot 
  pred_data_all_points = rbind( # do this for the points 
    data.frame(year = 2001,
               mean_all = 3.44, 
               # get the predicted value for the missing
               prop_lep = 
                 df_line[which(
                   df_line$mean_all == 3.44), "prop_lep"]
    ),
    data.frame(df %>% 
                 dplyr::select(-c(mean_lep)) 
    )
  ) %>% 
    mutate(
      predicted = as.factor(c("Predicted", rep("True", 20)))
    ) 
  
  predicted_proportions = ggplot() +
    geom_point(data = pred_data_all_points, 
               aes(x = mean_all, y = prop_lep, fill = predicted),
               shape = 21, size = 2.8) + 
    geom_text(data = pred_data_all_points, 
              aes(x = mean_all, y = prop_lep, label = year),
              hjust = 0, nudge_x = 0.05, size = 3.0) +
    geom_line(data = df_line,
              aes(x = mean_all, y = prop_lep),
              linetype = "dashed", colour = "grey50") + 
    scale_fill_manual(" ", values = c("purple1", "goldenrod2")) + 
    theme_bw() + 
    theme_mod_comp() +
    labs(x = "Mean number of lice per fish (all louse species)", 
         y = "Proportion of L. salmonis")
  
  ggplot2::ggsave(
    here::here("./figs/nonlinear-regression/nonlinear-model-predictions.png"),
    predicted_proportions,
    width = 6,
    height = 5,
    dpi = 600
  )
}

#############################
# nonlinear_regression_scenario() function
#############################
nonlinear_regression_scenario = function(df, file) {
  
  #' Use the cleaned SCFS wild lice data to run through a scenario option where
  #' we predict the number of lice in 2001 via non-linear regression 
  
  # prep the data from the raw df
  prepped_df = prep_data_nonlinear(df)
  
  # run regression 
  results_list = nonlinear_regression(prepped_df)
  
  # save model objects
  save_nonlinear_regression(results_list, file)
  
  # make prediction dataframe 
  pred_df = nonlinear_prediction(prepped_df, results_list[[1]])
  
  # plot results and save the plot
  nonlinear_plot(prepped_df, pred_df)
  
  # bind all of these results objects into a list
  results_list = list(prepped_df, results_list, pred_df)
  
  # save list of results
  saveRDS(results_list, paste0(file, 
                               "nonlinear_regression_full_analysis_object.rds"))
  
  return(results_list)
  
}

# beta regression option =======================================================

#############################
# beta_regression() function
#############################
beta_regression = function(df) {
  
  #' Take prepared dataset and fit using a beta regression 
  
  model = betareg::betareg(
    prop_lep ~ mean_all,
    data = df)
  
  # save object as nice neat file 
  coefs = broom::tidy(model)
  fitted_vals = broom::augment(model)
  model_vals = broom::glance(model)
  
  # list results objects
  results_list = list(model, coefs, fitted_vals, model_vals)
  
  return(results_list)
}

#############################
# save_beta_regression() function
#############################
save_beta_regression = function(results_list, file) {
  
  #' take output of the multiple model objects and save them to a useful 
  #' location for further inspection if needed. The model object is saved as a
  #' `.rds` file, the rest are saved as text files
  
  # save model object
  saveRDS(
    results_list[[1]], 
    here::here(paste0(file, "beta_regress_model_ob.rds"))
  )
  # save coefs
  readr::write_csv(
    results_list[[2]], 
    here::here(paste0(file, "beta_regress_coefs.csv"))
  )
  # save fitted values 
  readr::write_csv(
    results_list[[3]], 
    here::here(paste0(file, "beta_regress_fitted_vals.csv"))
  )
  # save glance object
  readr::write_csv(
    results_list[[4]],
    here::here(paste0(file, "beta_regress_glance_aic.csv"))
  )
}

#############################
# beta_prediction() function
#############################
beta_prediction = function(model) {
  
  #' Use the model object to predict across new values of all lice to get an 
  #' estimate for higher numbers of total lice - thus to predict 2001
  
  # predict the data back from the model 
  pred_data_points = data.frame(
    mean_all = seq(0,4,0.01)
  )
  
  # make prediction
  pred_prop_beta_quant = stats::predict(
    model, 
    pred_data_points,
    type = "quantile",
    at = c(0.0225, 0.5, 0.975)
  )
  
  # predicted dataframe
  predicted_beta_line = 
    data.frame(
      cbind(pred_data_points, pred_prop_beta_quant)
    ) %>% 
    dplyr::rename(lower = q_0.0225,
                  median = q_0.5,
                  upper = q_0.975)
  
  return(predicted_beta_line)
}

#############################
# beta_plot() function
#############################
beta_plot = function(df, predicted_beta_line) {
  
  #' Make and save a plot of the non-linear prediction based off the prediction
  #' for 2001 in this framework
  
  # make data for a plot
  pred_data_all_points = rbind(
    df %>% 
      dplyr::mutate(predicted = "Observed") %>% 
      dplyr::select(-mean_lep),
    data.frame(
      year = 2001,
      mean_all = 3.44,
      prop_lep = predicted_beta_line[which(
        predicted_beta_line$mean_all == 3.44), "median"],
      predicted = "Predicted"
    )
  )
  pred_data_all_points$predicted = as.factor(pred_data_all_points$predicted)
  
  # make plot
  predicted_proportions = ggplot2::ggplot() +
    geom_line(data = predicted_beta_line,
              aes(x = mean_all, y = median),
              colour = "grey65",
              size = 1.25) + 
    geom_ribbon(data = predicted_beta_line,
                aes(x = mean_all, ymin = lower, ymax = upper),
                fill = "steelblue2", alpha = 0.4) + 
    geom_point(data = pred_data_all_points, 
               aes(x = mean_all, y = prop_lep, fill = predicted),
               shape = 21, size = 2.8) + 
    geom_text(data = pred_data_all_points, 
              aes(x = mean_all, y = prop_lep, label = year),
              hjust = 0, nudge_x = 0.05, size = 3.0) +
    scale_fill_manual(" ", values = c("purple1", "goldenrod2")) + 
    theme_bw() + 
    theme_mod_comp() +
    labs(x = "Mean number of lice per fish (all louse species)", 
         y = "Proportion of L. salmonis") +
    ylim(0,1)
  
  # save object
  ggplot2::ggsave(filename = 
      here::here("./figs/beta-regression/beta-model-predictions.png"),
      plot = predicted_proportions,
      width = 6,
      height = 5,
      dpi = 600)
}

#############################
# beta_regression_scenario() function
#############################
beta_regression_scenario = function(df, file) {
  
  #' Use the cleaned SCFS wild lice data to run through a scenario option where
  #' we predict the number of lice in 2001 via beta regression 
  
  # prep the data from the raw df
  prepped_df = prep_data_nonlinear(df)
  
  # run regression 
  results_list = beta_regression(prepped_df)
  
  # save model objects
  save_beta_regression(results_list, file)
  
  # make prediction dataframe 
  pred_df = beta_prediction(results_list[[1]])
  
  # plot results and save the plot
  beta_plot(prepped_df, pred_df)
  
  # bind all of these results objects into a list
  results_list = list(prepped_df, results_list, pred_df)
  
  # save list of results
  saveRDS(results_list, paste0(file, 
                               "beta_regression_full_analysis_object.rds"))
  
  return(results_list)
  
}

