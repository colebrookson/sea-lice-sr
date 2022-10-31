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



# bati_df <- read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
# focal_time <- bati_df %>%
#   #dplyr::filter(year > 2017) %>%
#   dplyr::filter(month %in% c(3, 4)) %>%
#   dplyr::filter(ref %in% c(136, 728, 820, 821, 1059, 1144, 1586, 1618)) %>% 
#   dplyr::select(year, month, farm_name, inventory) 
# 
# focal_time$year <- as.factor(focal_time$year)
# focal_time$month <- as.factor(focal_time$month)
# focal_time$farm_name <- as.factor(focal_time$farm_name)
# 
# inventory_mean_by_farm <- focal_time %>%
#   dplyr::group_by(year, farm_name) %>%
#   dplyr::summarize(mean_inventory = mean(inventory, na.rm = TRUE))
# 
# inventory_sum_year <- inventory_mean_by_farm %>%
#   dplyr::group_by(year) %>%
#   dplyr::summarize(summed_inventory = sum(mean_inventory, na.rm = TRUE))
# 
# ggplot(inventory_sum_year) +
#   geom_line(aes(x = as.integer(as.character(year)), y = summed_inventory))
