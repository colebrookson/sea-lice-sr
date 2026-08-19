########## 
##########
# All functions for calculating the numbers of chalimus and writing out the 
# final dataframe for the count models 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-24
##########
##########

options(dplyr.summarise.inform = FALSE)

# testing
#chal_in = read_csv(here("./data/wild-lice-data/clean/chalimus-counted-lice.csv"))

#############################
# count_scenarios() function
#############################
count_scenarios = function(df) {
  
  #' For scenario 1 (logistic regression approach), count all the leps on each
  #' fish and then prepare that version of the dataset for analysis
  
  df = df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      all_leps_scen1_indiv = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, 
        lep_nongravid, lep_gravid, unid_pa, 
        chal_scen1_indiv_lep, cope_indiv_scen1_lep,
        mot_indiv_scen1_lep
      ),
      all_leps_scen1_year = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, unid_pa,
        lep_nongravid, lep_gravid, chal_scen1_year_lep, cope_year_scen1_lep,
        mot_year_scen1_lep
      ), 
      all_leps_scen2 = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, unid_pa,
        lep_nongravid, lep_gravid, mot_year_scen2_lep
      ),
      all_leps_scen3 = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, unid_pa,
        lep_nongravid, lep_gravid, mot_year_scen3_lep
      ),
      all_lice_scen4 = sum(unid_cope, lep_cope, cal_cope, chala, chalb, 
                           chal_unid, lep_pamale, lep_male, lep_nongravid,
                           lep_gravid, lep_pafemale, cal_mot, cal_gravid, 
                           unid_adult, unid_pa), 
      # check to make sure the addition is correct
      check_scen4_scen3 = ifelse(
        all_lice_scen4 >= all_leps_scen3, 
        TRUE,
        FALSE),
      date = lubridate::make_date(year, month, day)
    ) %>% 
    dplyr::mutate(
      week = lubridate::week(lubridate::ymd(date))
    ) %>% 
    dplyr::filter(
      week %notin% c(9, 28, 33)
    )
  
  if(!all(df$check_scen4_scen3)) {
    stop("ERROR - column addition not working as expected")
  }
  
  df = df %>% 
    dplyr::select(-c(check_scen4_scen3))
  
  return(df)
}

#############################
# scenario1_indiv() function
#############################
write_option_dfs = function(df, path) {
  
  #' Write out the df when it's ready
  
  readr::write_csv(df, path)
}

#############################
# scenario1_indiv() function
#############################
scenario1_indiv = function(df, path) {
  
  #' Return a version of the dataframe based on the scen1 with default to 
  #' individual-level 
  
  # start with raw df
  count_df = count_scenarios(df)
  
  count_df = count_df %>% 
    # dplyr::select(
    #   year, week, date, farm_name, all_leps_scen1_indiv
    # ) %>% 
    dplyr::rename(all_lep = all_leps_scen1_indiv)
  
  write_option_dfs(count_df, path)
  
  return(count_df)
}

#############################
# scenario1_year() function
#############################
scenario1_year = function(df, path) {
  
  #' Return a version of the dataframe based on the scen1 with default to 
  #' year-level 
  
  # start with raw df
  count_df = count_scenarios(df)
  
  count_df = count_df %>% 
    # dplyr::select(
    #   year, week, date, farm_name, all_leps_scen1_indiv
    # ) %>% 
    dplyr::rename(all_lep = all_leps_scen1_year)
  
  write_option_dfs(count_df, path)
  
  return(count_df)
}

#############################
# scenario2() function
#############################
scenario2 = function(df, path) {
  
  #' Return a version of the dataframe based on the scenario 2
  
  # start with raw df
  count_df = count_scenarios(df)
  
  count_df = count_df  %>% 
    # dplyr::select(
    #   year, week, date, farm_name, all_leps_scen1_indiv
    # ) %>% 
    dplyr::rename(all_lep = all_leps_scen2)
  
  write_option_dfs(count_df, path)
  
  return(count_df)
}

#############################
# scenario3() function
#############################
scenario3 = function(df, path) {
  
  #' Return a version of the dataframe based on the scenario 3
  
  # start with raw df
  count_df = count_scenarios(df)
  
  count_df = count_df %>% 
    # dplyr::select(
    #   year, week, date, farm_name, all_leps_scen1_indiv
    # ) %>% 
    dplyr::rename(all_lep = all_leps_scen3)
  
  write_option_dfs(count_df, path)
  
  return(count_df)
}

#############################
# scenario4() function
#############################
scenario4 = function(df, path) {
  
  #' Return a version of the dataframe based on the scenario 3
  
  # start with raw df
  count_df = count_scenarios(df)
  
  count_df = count_df %>% 
    # dplyr::select(
    #   year, week, date, farm_name, all_leps_scen1_indiv
    # ) %>% 
    dplyr::rename(all_lep = all_lice_scen4)
  
  write_option_dfs(count_df, path)
  
  return(count_df)
}
