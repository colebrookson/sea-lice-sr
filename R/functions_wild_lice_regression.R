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

# set up functions =============================================================

#############################
# read_data_clean_scfs() function
#############################
read_data_clean_scfs = function(file) {
  
  #' read in clean file to do analysis on 
  
  readr::read_csv(file)
  
}

#############################
# prep_data_mot() function
#############################
prep_data_mot = function(df) {
  
  #' Take in raw dataframe and cut it down to size, making two dataframes, one
  #' with data from 2001 onwards and one with only 2001 for comparison
  
  df_subset = df %>% 
    dplyr::select(year, prop_lep_mot, all_mot, lep_mot)
  
  df_2002_onwards = df_subset %>% 
    dplyr::filter(year > 2001)
  
  df_2001 = df_subset %>% 
    dplyr::filter(year == 2001)
  
  # list up return value 
  return_list = list(df_2001, df_2002_onwards)
  
  return(return_list)
}

#############################
# prep_data_cope() function
#############################
prep_data_cope =function(df) {
  
  #' Prep data on copepeodites to model 
  
  df_subset = df %>% 
    dplyr::select(year, prop_lep_cope, all_cope)
  
  df_2005_onwards = df_subset %>% 
    dplyr::filter(year > 2004)
  
  df_2002_2004 = df_subset %>% 
    dplyr::filter(year > 2001 & year < 2005)
}
  

