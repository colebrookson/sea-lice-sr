########## 
##########
# All functions for cleaning and preparing data from 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-20
##########
##########

# global functions =============================================================
options(dplyr.summarise.inform = FALSE)

get_data_scfs = function(file) {
  
  #' Read in data from Salmon Coast Field Station sampling of wild lice
  
  readr::read_csv(file)
}

clean_scfs_data = function(df)