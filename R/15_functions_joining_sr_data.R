########## 
##########
# Do the stock recruit models 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

library(tidyverse)
library(here)

# take the various objects and join them together 
prep_c_estimate = function(df, focal_scen, focal_min_pop) {
  
  #' Take in the dataframe, populate it with columns denoting values of interest
  #' and return that 
  
  df = df %>% 
    dplyr::mutate(
      scenario = focal_scen,
      min_pop = focal_min_pop
    )
}


join_c_estimates = function(df_1_in, df_1_yr, df_2, df_3, df_4) {
  
  #' Take in all dataframes, make them comoparable with columns, and join them 
  #' into one dataframe
  
  df_1_in = prep_c_estimate(df, )
}

# write out the joined up material 

# make relevant plots to compare across the various values 


