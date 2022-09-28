########## 
##########
# Do the stock recruit models 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

#############################
# prep_c_estimate() function 
#############################
prep_df_estimate = function(df, focal_scen, focal_min_pop) {
  
  #' Take in the dataframe, populate it with columns denoting values of interest
  #' and return that 
  
  df = df %>% 
    dplyr::mutate(
      scenario = focal_scen,
      min_pop = focal_min_pop
    )
}

#############################
# collect_c_estimates() function 
#############################
collect_estimates = function(df_1_in_20, df_1_in_3, df_1_yr_20, df_1_yr_3, 
                               df_2_20, df_2_3, df_3_20, df_3_3, df_4_20, 
                               df_4_3, output_path) {
  
  #' Take in all dataframes and join them for figure making 

  # bind all different values together 
  df_1_in_both = rbind(prep_df_estimate(df_1_in_20, "scenario-1-in", 20), 
                       prep_df_estimate(df_1_in_3, "scenario-1-in", 3))
  df_1_yr_both = rbind(prep_df_estimate(df_1_yr_20, "scenario-1-yr", 20), 
                       prep_df_estimate(df_1_yr_3, "scenario-1-yr", 3))
  df_2_both = rbind(prep_df_estimate(df_2_20, "scenario-2", 20), 
                    prep_df_estimate(df_2_3, "scenario-2", 3))
  df_3_both = rbind(prep_df_estimate(df_3_20, "scenario-3", 20), 
                    prep_df_estimate(df_3_3, "scenario-3", 3))
  df_4_both = rbind(prep_df_estimate(df_4_20, "scenario-4", 20), 
                    prep_df_estimate(df_4_3, "scenario-4", 3))
  
  # bind all these dataframes together
  df_all = rbind(
    df_1_in_both, df_1_yr_both, df_2_both, df_3_both, df_4_both
  )
  
  readr::write_csv(df_all, output_path)
  
  return(df_all)
}

# make relevant plots to compare across the various values 

# library(tidyverse)
# library(here)
# 
# df_20 = read_csv(here::here(paste0(
#   "./outputs/model-outputs/stock-recruit-models/",
#   "scen4/twenty-pairs/estimate-of-c.csv"
# )))
# df_3 = read_csv(here::here(paste0(
#   "./outputs/model-outputs/stock-recruit-models/",
#   "scen4/three-pairs/estimate-of-c.csv"
# )))

df_mort = read_csv(here::here("./outputs/model-outputs/stock-recruit-models/scen4/three-pairs/mortality-estimates.csv"))
df_future = read_csv(here::here("./outputs/model-outputs/stock-recruit-models/scen4/three-pairs/future-predicted-mortality-estimates.csv"))
