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
# 
# library(here)
# library(tidyverse)
# library(wesanderson)
# library(patchwork)
# 
# farm_df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
# all_scen_lice = read_csv(here("./data/wild-lice-data/clean/all-scenario-yearly-lice-per-fish-estimates.csv"))
# df = read_csv(here("./data/prepped-data/scfs-regression-scen2.csv"))

#############################
# make_farm_groupings() function
#############################
make_farm_groupings = function(farm_df) {
  
  #' Make three groupings of the farms - all the farms, the KTC farms, 
  #' and the HSD Triangle Farms, return all three of these dataframes 
  #' grouped by years 
  
  all_farms <- farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
      all_leps = mean(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      log_all_farm_leps = base::log10(all_leps)
    ) %>% 
    dplyr::select(-all_leps)
  
  # make ktc farm df
  ktc_farms <- farm_df %>%
    dplyr::filter(year > 2000) %>%
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>%
    dplyr::filter(ktc == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      ktc_leps = mean(lep_tot, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      log_ktc_leps = log10(ktc_leps)
    ) %>%
    dplyr::select(-ktc_leps)

  # make the hsd farms df
  hsd_farms <- farm_df %>%
    dplyr::filter(year > 2000) %>%
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>%
    dplyr::filter(hump_sarg_doc == 1) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      hsd_leps = mean(lep_tot, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      log_hsd_leps = log10(hsd_leps)
    ) %>%
    dplyr::select(-hsd_leps)

  # bind together all farm combos
  all_group_farms <- as_tibble(cbind(
    all_farms,
    ktc_farms %>%
      dplyr::select(-year),
    hsd_farms %>%
      dplyr::select(-year)
  ))
  
  return(all_group_farms)
  
}

#############################
# reshape_scenario_lice() function
#############################
reshape_scenario_lice = function(all_scen_lice) {
  
  #' Take the values and put them in wide format for easier regression
  
  # take just the fit value here 
  fit_lice = all_scen_lice %>% 
    dplyr::select(-c(lower, upper)) %>% 
    dplyr::mutate(scenario = as.factor(scenario)) %>% 
    # rowwise, add in a log fit value
    dplyr::rowwise() %>% 
    dplyr::mutate(
      log_fit = log10(fit)
    ) %>% 
    dplyr::select(-fit)
  
  # pivot wider for ease of model use 
  fit_wide =
    tidyr::pivot_wider(
    fit_lice,
    names_from = scenario,
    values_from = c(log_fit)
    )
  
  return(fit_wide)
}

#############################
# wild_farm_regression() function
#############################
wild_farm_regression = function(all_group_farms, wide_lice, 
                                mod_path, fig_path, data_path) {
  
  #' Perform regression between all 5 options of logged lice on farms, and 
  #' all three log versions of the farm lice
  
  # get the names of the columns to be regressed
  all_names_farm = names(all_group_farms)
  all_names_wild = names(wide_lice)
  farm_cols = all_names_farm[2:length(all_names_farm)]
  wild_cols = all_names_wild[2:length(all_names_wild)]
  
  # make matching vectors for easier plotting
  farm_cols_matching = dplyr::as_tibble(data.frame(
    variable = farm_cols,
    name = c("All Farms", "Knight-Tribune Farms", 
             "Humphrey, Sargeaunt, & Doctors Farms"),
    # make a column for which shape to use in the plot 
    num = c(21, 22, 23)
  ))
  wild_cols_matching = dplyr::as_tibble(data.frame(
    variable = wild_cols,
    name = c("Scenario 1 (Individual)", "Scenario 1 (Year)", "Scenario 2", 
             "Scenario 3", "Scenario 4"),
    # make a column for which fill colour to use in the plot
    num = c(1:5)
  ))
  
  # make a list for the ggplot objects and all the model objects
  mod_obs = vector(mode = "list",
                   length = (length(farm_cols) * length(wild_cols)))
  plot_obs = vector(mode = "list", 
                    length = (length(farm_cols) * length(wild_cols)))
  df_obs = vector(mode = "list",
                  length = (length(farm_cols) * length(wild_cols)))

  # loop through each option in the farm_cols and in the wild_cols 
  list_loc = 1L
  for(i in seq_len(length(farm_cols))) {
    for(j in seq_len(length(wild_cols))) {
      
      farm = farm_cols[i]; wild = wild_cols[j]
      # first put the two columns beside each other 
      mod_df = cbind(
        all_group_farms[ ,c("year", farm)], wide_lice[, wild])
      names(mod_df) = c("year", "farm", "wild")
      mod_df$wild_scenario = wild
      mod_df$farm_group = farm
      
      # save df ob
      df_obs[[list_loc]] <- mod_df
      
      # now fit the model
      mod = stats::lm(
        wild ~ farm, 
        data = mod_df
      )
      # save into the list
      mod_obs[[list_loc]] <- mod
      
      # extract vals of use
      coefs = broom::tidy(mod)
      fitted_vals = broom::augment(mod)
      model_vals = broom::glance(mod)
      
      # save all objects
      saveRDS(mod,
              paste0(mod_path, "model_object_", farm, "_", wild, ".rds"))
      readr::write_csv(coefs,
              paste0(mod_path, "coeffs_", farm, "_", wild, ".csv"))
      readr::write_csv(fitted_vals,
                       paste0(mod_path, "fiited_vals_", farm, "_", wild, ".csv"))
      readr::write_csv(model_vals,
                       paste0(mod_path, "glance_aic_", farm, "_", wild, ".csv"))
      
      # set shape for the farm options, fill for the scenario options
      shape_val = farm_cols_matching$num[which(
        farm_cols_matching$variable == farm)]
      palette = as.vector(wesanderson::wes_palette("Royal2"))
      fill_val = palette[
        wild_cols_matching$num[which(wild_cols_matching$variable == wild)]
      ]
      
      # extract adjusted r-squared to put on plot pane
      rsq = model_vals$adj.r.squared
      
      # make plot for this particular model 
      plot <- ggplot(data = mod_df, aes(x = farm, y = wild)) + 
        geom_point(
          # use the shape and colour to denote which are which
          fill = fill_val, shape = shape_val,
          colour = "black", size = 5) +
        stat_smooth(method = stats::lm, formula = y ~ x,
                    colour = "black", alpha = 0.2) +
        ggthemes::theme_base() +
        labs(
          x = paste0("Lice on Farmed Fish (Log10) - ",
                     farm_cols_matching$name[which(
                       farm_cols_matching$variable == farm
                     )]), 
          y = paste0("Lice on Wild Fish (Log10) - ",
                     wild_cols_matching$name[which(
                       wild_cols_matching$variable == wild
                     )])
        ) + 
        annotate(geom = "text", 
                 x = min(mod_df$farm, na.rm = TRUE), 
                 y = max(mod_df$wild, na.rm = TRUE), 
                 label = paste("R^2 ==", round(rsq, 2)),
                 size = 5,
                 parse = TRUE,
                 hjust = -0.5) + 
        theme(
          legend.position = "none",
          axis.title = element_text(size = 14)
        ) +
        ylim(c(-1.5, 1.2))
      
      # save plot
      ggsave(paste0(
        fig_path, farm, "_", wild, ".png"),
        plot,
        dpi = 600)

      # keep the plot object in the list 
      plot_obs[[list_loc]] <- plot
      
      list_loc = list_loc + 1
    }
  }
  
  # once loop is done, take all plot objects and stich them together
  all_farms_plot = plot_obs[[1]] | plot_obs[[2]] | plot_obs[[3]] | plot_obs[[4]] | 
    plot_obs[[5]]
  ggsave(paste0(
    fig_path, "all_farm_scenario_comparison_regression_plots.png"),
    all_farms_plot,
    dpi = 600)
  
  ktc_farms_plot = plot_obs[[6]] | plot_obs[[7]] | plot_obs[[8]] | plot_obs[[9]] | 
    plot_obs[[10]]
  ggsave(paste0(
    fig_path, "ktc_farm_scenario_comparison_regression_plots.png"),
    ktc_farms_plot,
    dpi = 600)
  
  hsd_farms_plot = plot_obs[[11]] | plot_obs[[12]] | plot_obs[[13]] | plot_obs[[14]] | 
    plot_obs[[15]]
  ggsave(paste0(
    fig_path, "hsd_farm_scenario_comparison_regression_plots.png"),
    hsd_farms_plot,
    dpi = 600)
  
  scen1ind_plot = plot_obs[[1]] | plot_obs[[6]] | plot_obs[[11]]
  ggsave(paste0(
    fig_path, "scen_1ind_comparison_regression_plots.png"),
    scen1ind_plot,
    dpi = 600)
  
  scen1yr_plot = plot_obs[[2]] | plot_obs[[7]] | plot_obs[[12]]
  ggsave(paste0(
    fig_path, "scen_1yr_comparison_regression_plots.png"),
    scen1yr_plot,
    dpi = 600)
  
  scen2_plot = plot_obs[[3]] | plot_obs[[8]] | plot_obs[[13]]
  ggsave(paste0(
    fig_path, "scen_2_comparison_regression_plots.png"),
    scen2_plot,
    dpi = 600)
  
  scen3_plot = plot_obs[[4]] | plot_obs[[9]] | plot_obs[[14]]
  ggsave(paste0(
    fig_path, "scen_3_comparison_regression_plots.png"),
    scen3_plot,
    dpi = 600)
  
  scen4_plot = plot_obs[[5]] | plot_obs[[10]] | plot_obs[[15]]
  ggsave(paste0(
    fig_path, "scen_4_comparison_regression_plots.png"),
    scen4_plot,
    dpi = 600)
  
  all_plots = 
    (plot_obs[[1]] | plot_obs[[2]] | plot_obs[[3]] | plot_obs[[4]] | 
       plot_obs[[5]]) /
    (plot_obs[[6]] | plot_obs[[7]] | plot_obs[[8]] | plot_obs[[9]] | 
       plot_obs[[10]]) /
    (plot_obs[[11]] | plot_obs[[12]] | plot_obs[[13]] | plot_obs[[14]] | 
       plot_obs[[15]]) 
  ggsave(paste0(
    fig_path, "all_scenario_farm_regression_plots.png"),
    all_plots,
    dpi = 600)

  # join all the prediction df's together and write it out
  df = df_obs[[1]]
  for(i in 2:length(df_obs)) {
    df = rbind(df, df_obs[[i]])
  }
  
  readr::write_csv(df, data_path)
  
  # return the prediction data for use in the stock-recruit part
  return(df)
}

#############################
# execute_wild_farm_regressions() function
#############################
execute_wild_farm_regressions = function(farm_df, all_scen_lice,
                                         mod_path, fig_path, data_path) {
  
  #' Use defined helper functions to go through the different combinations 
  #' regress them all against each other, then plot all the results
  
  # take the farm dataframe and make the groupings necessary for the regression
  all_group_farms = make_farm_groupings(farm_df)
  
  # take the lice df and reshape it for easier use
  wide_lice = reshape_scenario_lice(all_scen_lice)

  # make & save the regressions, then save the plots
  df = wild_farm_regression(all_group_farms, wide_lice,
                       mod_path, fig_path, data_path)

  return(df)
}
# mod_path = here::here(
#   "./outputs/model-outputs/wild-farm-regressions/"
# )
# fig_path = here::here(
#   "./figs/wild-farm-regressions/"
# )
# execute_wild_farm_regressions(farm_df, all_scen_lice, mod_path, fig_path)
