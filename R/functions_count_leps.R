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
  
  df %>% 
    dplyr::mutate(
      all_leps_scen1_indiv = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, 
        lep_nongravid, lep_gravid, chal_scen1_indiv_lep, cope_indiv_scen1_lep,
        mot_indiv_scen1_lep
      ),
      all_leps_scen1_year = sum(
        lep_cope, lep_pamale, lep_pafemale, lep_male, 
        lep_nongravid, lep_gravid, chal_scen1_year_lep, cope_year_scen1_lep,
        mot_year_scen1_lep
      ), 
      all_leps_scen2 = sum(
        
      )
      
    )
}