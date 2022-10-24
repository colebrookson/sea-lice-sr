########## 
##########
# Plot output from all the options we have 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

# library(here)
# library(tidyverse)
# library(ggthemes)
# 
# df_c = read_csv(here("./outputs/model-outputs/stock-recruit-models/joined-c-estimates.csv"))
# df_est = read_csv(here("./outputs/model-outputs/stock-recruit-models/joined-mortality-estimates.csv"))
# df_fut = read_csv(here("./outputs/model-outputs/stock-recruit-models/joined-future-mortality-estimates.csv"))
# 
# df_c$min_pop = as.factor(df_c$min_pop)
# df_c_wide = df_c %>% 
#   tidyr::pivot_wider(., 
#                      id_cols = c(scenario, min_pop),
#                      names_from = value,
#                      values_from = c)

#############################
# prep_c_plot_df() function 
#############################
prep_c_plot_df = function(df_c) {
  
  #' Take in dataframe that's not ready and make a couple of small changes
  
  df_c_wide = df_c %>% 
    tidyr::pivot_wider(., 
                       id_cols = c(scenario, min_pop),
                       names_from = value,
                       values_from = c)
  
  return(df_c_wide)
}

#############################
# make_c_plot() function 
#############################
make_c_plot = function(df_c, output_path) {
  
  #' Make the plot of the focal dataframe 
  
  df_c_wide = prep_c_plot_df(df_c) %>% 
    dplyr::mutate(
      min_pop = as.factor(min_pop),
      scenario = as.factor(scenario)
    )
  
  c_plot = ggplot(data = df_c_wide, aes(x = scenario, y = -MLE, 
                                        fill = min_pop)) + 
    geom_errorbar(mapping = aes(ymin = -lower, ymax = -upper),
                  position = position_dodge(width = 0.5),
                  width = 0) +
    geom_point(position = position_dodge(width = 0.5), shape = 21, size = 3) +
    ylim(c(0, 0.5)) + 
    ggthemes::theme_base() + 
    labs(x = "Scenario", y = "Estiamted Effect (c) - MLE and 95% CI's") + 
    scale_fill_manual("Min. # of S-R \npairs per pop'n", 
                      values = c("purple", "goldenrod2")) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)
    ) + 
    scale_x_discrete(
      labels = c("Scenario 1 (Indiv.)", "Scenario 1 (Year)", "Scenario 2", 
                 "Scenario 3", "Scenario 4")
    )
  
  ggsave(
    paste0(output_path, "c-estimate-plot.png"),
    c_plot,
    height = 6, width = 8,
    dpi = 600
  )
}

#############################
# make_mortality_plot() function 
#############################
make_mortality_plot = function(df_fut, output_path) {
  
  #' Make plot of estimated and predicted mortality 

  # first make sure important values are factors
  df_fut$min_pop = as.factor(as.integer(df_fut$min_pop))
  df_fut$predict = as.factor(df_fut$predict)
  df_fut$scenario = as.factor(df_fut$scenario)
  df_fut$year = as.numeric(df_fut$year)

  # make the actual plot
  mort_plot = ggplot(data = df_fut, aes(x = year, y = MLE, fill = min_pop,
                                        shape = scenario,
                                        alpha = predict)) +
    geom_errorbar(mapping = aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5),
                  width = 0) +
    geom_point(position = position_dodge(width = 0.5), size = 3) +
    facet_wrap(~scenario) +
    ggthemes::theme_base() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2),
      strip.text.x = element_blank()
    ) +
    scale_x_continuous(breaks = c(2002:2021),
                       labels = c(2001:2020)) +
    labs(x = "Return Year", y = "Estimated Mortality - MLE and 95% CI's") +
    scale_shape_manual( 
      "Scenario",
      values = c(21:25),
      labels = c("Scenario 1 (Indiv.)", "Scenario 1 (Year)",
                 "Scenario 2", "Scenario 3", "Scenario 4")) +
    scale_fill_manual(
      "Min. # of S-R \npairs per pop'n",
      values = c("purple", "goldenrod2")
    ) +
    scale_alpha_manual(
      "Estimated vs. Predicted",
      values = c(1, 0.4)
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21, size = 3))
    )

  ggsave(
    paste0(output_path, "mortality-plot.png"),
    mort_plot,
    height = 6, width = 12,
    dpi = 600
  )
  
  # make version of the plot with only ktf and 3 pairs 
  df_fut_focal = data.frame(df_fut) %>% 
    dplyr::filter(
      scenario == "scenario-1-yr",
      min_pop == 3
    )
  
  focal_only = ggplot(data = df_fut_focal) + 
    geom_errorbar(aes(x = year, ymin = lower, ymax = upper),
                  width = 0) + 
    geom_point(aes(x = year, y = MLE),
               size = 4, colour = "black", fill = "purple", shape = 21) + 
    scale_x_continuous(breaks = c(2002:2021),
                       labels = c(2001:2020)) +
    labs(x = "Return Year", y = "Estimated Mortality - MLE and 95% CI's") + 
    ggthemes::theme_base()  + 
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)
    )
  
  ggsave(
    paste0(output_path, "focal-mortality-plot.png"),
    focal_only,
    dpi = 600
  )
  
}

#############################
# focal_plot() function 
#############################
focal_plot = function(df_fut, output_path) {
  
  #' use the bound mortality estimates to make plot of the focal scenario 
  
  # first make sure important values are factors
  df_fut$min_pop = as.factor(df_fut$min_pop)
  df_fut$predict = as.factor(df_fut$predict)
  df_fut$scenario = as.factor(df_fut$scenario)
  df_fut$year = as.numeric(df_fut$year)
  
  df_fut_focal = as_tibble(df_fut) %>%
    dplyr::filter(
      scenario == "scenario-1-yr",
      min_pop == 3
    ) 
  
  focal_mort = ggplot(data = df_fut_focal) + 
    geom_line(aes(x = year, y = MLE)) +
    geom_point(aes(x = year, y = MLE, fill = predict),
               size = 4, shape = 21,  colour = "black") +
    scale_x_continuous(breaks = c(2002:2021)) +
    labs(x = "Return Year", y = "Estimated Mortality - MLE and 95% CI's") +
    ggthemes::theme_base() +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.2)
    ) +
    scale_fill_manual("Predicted vs. Estimated",
                      values = c("purple", "goldenrod2"))
  
  ggsave(
    paste0(output_path, "focal-scenario-mortality-plot.png"),
    focal_mort,
    dpi = 600
  )

  
}

prep_df_for_timeseries = function(df) {
  
  #' Take in cleaned joined data from the farms, and plot both inventory and 
  #' louse abundance 
  
  # shrink to size first 
  df = df %>% 
    dplyr::filter(
      year > 2000,
      year < 2021,
      month %in% c(3, 4)
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      day = 1,
      date = lubridate::make_date(year, month)
    ) %>% 
    dplyr::mutate(
      year = as.factor(year), 
      month = as.factor(month)
    )
  
  # cut dataframe into pieces according to farm arrangement 
  df_all_farms = df %>% 
    dplyr::group_by(year, month) %>% 
    dplyr::summarize(
      sum_inventory = sum(inventory, na.rm = TRUE),
      sum_lice = sum(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
      sum_inventory = mean(sum_inventory, na.rm = TRUE),
      sum_lice = mean(sum_lice, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      farm = "all"
    )
  
  df_ktf = df %>% 
    dplyr::filter(
      ktf == 1
    ) %>% 
    dplyr::group_by(year, month) %>% 
    dplyr::summarize(
      sum_inventory = sum(inventory, na.rm = TRUE),    
      sum_lice = sum(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
      sum_inventory = mean(sum_inventory, na.rm = TRUE),
      sum_lice = mean(sum_lice, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      farm = "ktf"
    )
  
  df_hsd = df %>% 
    dplyr::filter(
      hump_sarg_doc == 1
    ) %>% 
    dplyr::group_by(year, month) %>% 
    dplyr::summarize(
      sum_inventory = sum(inventory, na.rm = TRUE),
      sum_lice = sum(lep_tot, na.rm = TRUE)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
      sum_inventory = mean(sum_inventory, na.rm = TRUE),
      sum_lice = mean(sum_lice, na.rm = TRUE)
    ) %>% 
    dplyr::mutate(
      year = as.numeric(as.character(year)),
      farm = "hsd"
    ) 
  
  df_farms = rbind(df_all_farms, df_hsd, df_ktf)
  
  return(df_farms)
}

plot_timeseries = function(df, output_path) {
  
  #' Plot the two timeseries 
  
  df_farms = prep_df_for_timeseries(df)
  
  salmon = ggplot(data = df_farms) + 
    geom_line(aes(x = year, y = sum_inventory/1000000, group = farm)) +
    geom_point(aes(x = year, y = sum_inventory/1000000, fill = farm), 
               shape = 21, size = 3) + 
    ggthemes::theme_base() + 
    labs(x = "Year", y = "Number of Farmed \n Salmon (millions)") + 
    scale_fill_manual(
      "Farm Groupings", 
      values = wesanderson::wes_palette("FantasticFox1", 3),
      labels = c("All Farms", "Humphrey, Sargeaunt, & Doctors", "Knight Inlet-Tribune Channel-Fife Sound")
    ) + 
    scale_x_continuous(breaks = c(2001:2020), labels = c(2001:2020)) +
    theme(
      axis.text.x = element_blank(),
      legend.position = c(0.7, 0.7),
      axis.title.x = element_blank()
    )
  
  lice = ggplot(data = df_farms) + 
    geom_line(aes(x = year, y = sum_lice/1000000, group = farm)) +
    geom_point(aes(x = year, y = sum_lice/1000000, fill = farm), 
               shape = 21, size = 3) + 
    ggthemes::theme_base() + 
    labs(x = "Year", y = "Lice on Farmed \n Salmon (millions)") + 
    scale_fill_manual(
      "Farm Groupings", 
      values = wesanderson::wes_palette("FantasticFox1", 3),
      labels = c("All Farms", "Humphrey, Sargeaunt, & Doctors", "Knight Inlet-Tribune Channel-Fife Sound")
    ) + 
    scale_x_continuous(breaks = c(2001:2020), labels = c(2001:2020)) +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "none",
    )
  
  final = salmon / lice
  
  ggsave(
    paste0(output_path, "timeseries-lice-fish-abundance.png"),
    final,
    height = 7, width = 10, 
    dpi = 600
  )
  
  # make dataset to plot a timeseries of just ktf
  only_ktf = rbind(
    #inventory part of it
    df_farms %>% 
    dplyr::filter(farm == "ktf") %>% 
    dplyr::select(year, sum_inventory) %>% 
    dplyr::rename(val = sum_inventory) %>% 
    dplyr::mutate(type = "inventory"), 
    df_farms %>% 
      dplyr::filter(farm == "ktf") %>% 
      dplyr::select(year, sum_lice) %>% 
      dplyr::rename(val = sum_lice) %>% 
      dplyr::mutate(type = "lice")
  ) %>% 
    dplyr::mutate(type = as.factor(type))
   
  
  ktf_only_plot = ggplot(data = only_ktf) + 
    geom_line(aes(x = year, y = val/1000000, group = type,
                  linetype = type)) +
    geom_point(aes(x = year, y = val/1000000, fill = type), 
                size = 4, shape = 21) + 
    ggthemes::theme_base() + 
    labs(x = "Year", y = "Numbers (millions)") + 
    scale_fill_manual(
      "Farm Groupings", 
      values = wesanderson::wes_palette("FantasticFox1", 3),
      labels = c("Farmed Salmon", "Lice on Farmed\nSalmon")
    ) + 
    scale_x_continuous(breaks = c(2001:2020), labels = c(2001:2020)) +
    theme(
      axis.text.x = element_text(angle = 90)
    ) +
    guides(
      linetype = "none"
    )
  
  ggsave(
    paste0(output_path, "timeseries-ktf-only.png"),
    ktf_only_plot,
    height = 7, width = 10, 
    dpi = 600
  )
  
}


# 
# df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
# df_fut = read_csv(here("./outputs/model-outputs/stock-recruit-models/joined-future-mortality-estimates.csv"))
# 
# 
# 
# 
# 
# 
# 
# 
# ggplot() + 
#   geom_point(data = df_all_farms, aes(x = year, y = sum_inventory/1000000),
#              shape = 21, colour = "black", fill = "red") + 
#   geom_line(data = df_all_farms, aes(x = year, y = sum_inventory/1000000)) +
#   geom_point(data = df_ktf, aes(x = year, y = sum_inventory/1000000)) +
#   geom_line(data = df_ktf, aes(x = year, y = sum_inventory/1000000)) + 
#   geom_point(data = df_hsd, aes(x = year, y = sum_inventory/1000000)) +
#   geom_line(data = df_hsd, aes(x = year, y = sum_inventory/1000000))
#   
#   
#   
# 
# 
# 
# 
# 
# 
# 
# df_all_lice = df %>% 
#   dplyr::group_by(year) %>% 
#   dplyr::summarize(
#     sum_lice = sum(lep_tot, na.rm = TRUE)
#   ) %>% 
#   dplyr::mutate(
#     year = as.numeric(as.character(year))
#   )
# 
# 
# 
# ggplot(data = df_all_lice) + 
#   geom_point(aes(x = year, y = sum_lice/1000000)) +
#   geom_line(aes(x = year, y = sum_lice/1000000))
# 
# bati_df = read_csv(here("./data/farm-data/clean/bati-data-clean.csv"))
# 
# bati_df = bati_df %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(
#     date = lubridate::make_date(year, month)
#   ) %>% 
#   dplyr::group_by(date) %>% 
#   dplyr::summarize(
#     sum_inventory = sum(inventory, na.rm = TRUE)
#   )
#   
# ggplot(data = bati_df) + 
#   geom_point(aes(x = date, y = sum_inventory/1000000)) + 
#   geom_line(aes(x = date, y = sum_inventory/1000000))-
# 
# 
# marty_df = read_csv(here("./data/farm-data/clean/marty-data-clean.csv"))
# 
# marty_df = marty_df %>%
#   dplyr::filter(
#     year > 2000
#   ) %>% 
#   dplyr::rowwise() %>% 
#   dplyr::mutate(
#     date = lubridate::make_date(year, month)
#   ) %>% 
#   dplyr::group_by(date) %>% 
#   dplyr::summarize(
#     sum_inventory = sum(inventory, na.rm = TRUE)
#   ) 
# 
# ggplot(data = marty_df) + 
#   geom_point(aes(x = date, y = sum_inventory/1000000)) + 
  # geom_line(aes(x = date, y = sum_inventory/1000000))
