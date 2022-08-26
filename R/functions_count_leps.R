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

library(tidyverse)
library(here)
df = read_csv(here("./data/wild-lice-data/clean/chalimus-counted-lice.csv"))

count_scenario1_indiv = function(df) {
  
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
        lep_nongravid, lep_gravid, unid_cope, chala, chalb, 
        chal_unid, mot_year_scen2_lep
      ),
      all_leps_scen3 = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, unid_pa,
        lep_nongravid, lep_gravid, unid_cope, chala, chalb, 
        chal_unid, mot_year_scen3_lep
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
   dplyr::select(
     year, month, day, date, all_leps_scen1_indiv, all_leps_scen1_year, 
     all_leps_scen2, all_leps_scen3, all_lice_scen4, check_scen4_scen3
   )
  
  if(!all(df$check_scen4_scen3)) {
    stop("ERROR - column addition not working as expected")
  }
  
  df = df %>% 
    dplyr::select(-c(check_scen4_scen3))
}