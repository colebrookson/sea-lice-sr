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

library(here)
library(tidyverse)
library(wesanderson)
library(patchwork)

farm_df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
all_scen_lice = read_csv(here("./data/wild-lice-data/clean/all-scenario-yearly-lice-per-fish-estimates.csv"))
df = read_csv(here("./data/prepped-data/scfs-regression-scen2.csv"))

make_farm_groupings = function(farm_df) {
  
  #' Make three groupings of the farms - all the farms, the KTC farms, 
  #' and the HSD Triangle Farms, return all three of these dataframes 
  #' grouped by years 
  
  all_farms = farm_df %>% 
    dplyr::filter(year > 2000) %>% 
    # keep only months that fish actually migrate through during
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(
      all_leps = mean(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      log_all_farm_leps = log10(all_leps)
    ) %>% 
    dplyr::select(-all_leps)
  
  # make ktc farm df 
  ktc_farms = farm_df %>% 
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
  hsd_farms = farm_df %>% 
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
  all_group_farms = cbind(
    all_farms,
    ktc_farms %>% 
      dplyr::select(-year),
    hsd_farms %>% 
      dplyr::select(-year)
  )
  
  return(all_group_farms)
  
}

reshape_scenario_lice = function(all_scen_lice) {
  
  #' Take the values and put them in wide format for easier regression
  
  # take just the fit value here 
  fit_lice = all_scen_lice %>% 
    dplyr::select(-c(farm_name, week, lower, upper)) %>% 
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




all_group_farms = make_farm_groupings(farm_df)

wide_lice = reshape_scenario_lice(all_scen_lice)

wild_farm_regression = function(all_group_farms, wide_lice, 
                                mod_path, fig_path) {
  
  #' Perform regression between all 5 options of logged lice on farms, and 
  #' all three log versions of the farm lice
  
  # get the names of the columns to be regressed
  farm_cols = names(all_group_farms)[2:length(names(all_group_farms))]
  wild_cols = names(wide_lice)[2:length(names(wide_lice))]
  
  # make matching vectors for easier plotting
  farm_cols_matching = data.frame(
    variable = farm_cols,
    name = c("All Farms", "Knight-Tribune Farms", 
             "Humphrey, Sargeaunt, & Doctors Farms"),
    # make a column for which shape to use in the plot 
    num = c(21, 22, 23)
  )
  wild_cols_matching = data.frame(
    variable = wild_cols,
    name = c("Scenario 1 (Individual)", "Scenario 1 (Year)", "Scenario 2", 
             "Scenario 3", "Scenario 4"),
    # make a column for which fill colour to use in the plot
    num = c(1:5)
    
  )
  
  # make a list for the ggplot objects and all the model objects
  mod_obs = vector(mode = "list",
                   length = length(farm_cols) * length(wild_cols))
  plot_obs = vector(mode = "list", 
                    length = length(farm_cols) * length(wild_cols))
  df_obs = vector(mode = "list",
                  length = length(farm_cols) * length(wild_cols))
  
  # loop through each option in the farm_cols and in the wild_cols 
  list_loc = 1
  for(i in farm_cols) {
    for(j in wild_cols) {
      
      # first put the two columns beside each other 
      mod_df = data.frame(all_group_farms[ ,i], wide_lice[, j])
      names(mod_df) = c("farm", "wild")
      
      # save df ob
      df_obs[[list_loc]] = mod_df
      
      # now fit the model
      mod = stats::lm(
        wild ~ farm, 
        data = mod_df
      )
      # save into the list
      mod_obs[[list_loc]] = mod
      
      # extract vals of use
      coefs = broom::tidy(mod)
      fitted_vals = broom::augment(mod)
      model_vals = broom::glance(mod)
      
      # # save all objects
      # saveRDS(mod, 
      #         paste0(mod_path, "model_object_", i, "_", j, ".rds"))
      # readr::write_csv(coefs, 
      #         paste0(mod_path, "coeffs_", i, "_", j, ".csv"))
      # readr::write_csv(fitted_vals, 
      #                  paste0(mod_path, "fiited_vals_", i, "_", j, ".csv"))
      # readr::write_csv(model_vals, 
      #                  paste0(mod_path, "glance_aic_", i, "_", j, ".csv"))
      
      # set shape for the farm options, fill for the scenario options
      shape_val = farm_cols_matching$num[which(farm_cols_matching$variable == i)]
      palette = wesanderson::wes_palette("Royal2")
      fill_val = palette[
        wild_cols_matching$num[which(wild_cols_matching$variable == j)]
      ]
      
      # extract adjusted r-squared to put on plot pane
      rsq = model_vals$adj.r.squared
      
      # make plot for this particular model 
      plot = ggplot(data = mod_df, aes(x = farm, y = wild)) + 
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
                       farm_cols_matching$variable == i
                     )]), 
          y = paste0("Lice on Wild Fish (Log10) - ",
                     wild_cols_matching$name[which(
                       wild_cols_matching$variable == j
                     )])
        ) + 
        annotate(geom = "text", 
                 x = min(mod_df$farm, na.rm = TRUE), 
                 y = max(mod_df$wild, na.rm = TRUE), 
                 label = paste("R^2 ==", round(rsq, 2)),
                 size = 5,
                 parse = TRUE,
                 hjust = 0.3) + 
        theme(
          legend.position = "none",
          axis.title = element_text(size = 14)
        )
      
      # # save plot
      # ggsave(paste0(
      #   fig_path, i, "_", j, ".png"),
      #   plot,
      #   dpi = 600)
      
      # keep the plot object in the list 
      plot_obs[[list_loc]] = plot
      
      list_loc = list_loc + 1
    }
  }
  
  # once loop is done, take all plot objects and stich them together
  all_plots = 
    (plot_obs[[1]] | plot_obs[[2]] | plot_obs[[3]] | plot_obs[[4]] | 
       plot_obs[[5]]) /
    (plot_obs[[6]] | plot_obs[[7]] | plot_obs[[8]] | plot_obs[[9]] | 
       plot_obs[[10]]) /
    (plot_obs[[11]] | plot_obs[[12]] | plot_obs[[13]] | plot_obs[[14]] | 
       plot_obs[[15]]) 
  
  # ggsave(paste0(
  #   fig_path, "all_scenario_farm_regression_plots.png"),
  #   all_plots,
  #   dpi = 600)

}


