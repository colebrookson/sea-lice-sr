########## 
##########
# All functions for testing the farm data to ensure the calculations are good
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-13
##########
##########

options(dplyr.summarise.inform = FALSE)

library(here)
library(tidyverse)

farm_df = read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))


#############################
# plot_predictions() function 
#############################
check_lep_tot_values = function(farm_df) {
  
  #' Use a simple but inelegant for loop to make sure the values of the lep_tot
  #' are correct - this is an essential value 
  
  # set a tolerance to deal with rounding problems 
  tol = 1e-5
  
  for(row in seq_len(nrow(farm_df))) {
    
    # if the value is NA, make sure at least one of the inventory or the lep_av 
    # are also NA
    if(is.na(farm_df[row, "lep_tot"])) {
      
      # if it is an NA make sure one of the other two are
      if(!is.na(farm_df[row, "inventory"]) & !is.na(farm_df[row, "lep_av"])) {
        
        stop("ERROR - lep_tot not calculated properly at row ", row)
        
      } 
    # if the value is real, ensure it's the correct value 
    } else if(
      abs(
        farm_df$lep_tot[row] - (farm_df$lep_av[row] * farm_df$inventory[row])
      ) > tol
    ) {
      stop("ERROR - lep_tot not calculated properly at row ", row)
    }
  }
}
check_lep_tot_values(farm_df)
