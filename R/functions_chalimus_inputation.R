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

# To compare across our different scenarios, we need to generate 
# individual-level counts of lice, so this will be done across our four 
# scenarios (plus the sub-levels of individual vs. year default approaches) in
# some of the scenarios. 

#############################
# get_averaged_proportions() function
#############################
get_averaged_proportions = function(df) {
  
  #' Average the proportions for the copes and the mots
  
  df = df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      avg_pred_prop_indiv_scen1 = mean(pred_prop_mot_indiv_scen1, 
                                       pred_prop_cope_indiv_scen1),
      avg_pred_prop_year_scen1 = mean(pred_prop_mot_year_scen1,
                                       pred_prop_cope_year_scen1)
      )
  
  return(df)
}

#############################
# get_averaged_proportions() function
#############################
count_chalimus = function(df) {
  
  #' make counts of all leps on the individual level for the individual-default
  #' approach, wherein chalimus are counted by averaging the proportion of 
  #' motile and copepodite expected L. salmonis 
  
  # set seed
  set.seed(1234)
  
  df = df %>% 
    # make the empty columns
    dplyr::mutate(
      chal_scen1_indiv_lep = 0,
      chal_scen1_indiv_cal = 0,
      chal_scen1_year_lep = 0,
      chal_scen1_year_cal = 0
    )
  
  # make vectors of columns to check and new columns to use
  use_cols = c("chala", "chalb", "chal_unid")
  new_cols = c("chal_scen1_indiv_lep", "chal_scen1_year_lep",
               "chal_scen1_indiv_cal", "chal_scen1_year_cal")
  scen_cols = c("avg_pred_prop_indiv_scen1", "avg_pred_prop_year_scen1")
  
  for(row in seq_len(nrow(df))) {
    for(col in 1:length(use_cols)) {
      
      # check if there are lice to count
      if(df[row, use_cols[col]] > 0) {
        # go through each of the two scenarios
        for(scen in 1:length(scen_cols)) {
          # for each louse make a draw
          for(louse in 1:max(df[row, use_cols[col]][1])) {
            draw = sample(c(1, 0),
                          size = 1,
                          # draw with probability of that line for leps
                          # and 1 - that probability for cals (cals would be 0)
                          prob = c(
                            df[row, scen_cols[scen]],
                            1 - (df[row, scen_cols[scen]])))
            if(draw == 1) {
              # these are the leps columns
              df[row, new_cols[scen]] = df[row, new_cols[scen]] + 1
            } else if(draw == 0) {
              # these are the cals columns
              df[row, new_cols[scen + 2]] = df[row, new_cols[scen + 2]] + 1
            }
          }
        }
      }
    }
  }
  
  # to check that it all worked correctly, make a temporary dataframe and 
  # check that the sum of the new columns is exactly equal for each row 
  # to the sum of the old columns 
  temp = df %>% 
    dplyr::rowwise() %>% 
    # make columns to sum the old and 2 new versions
    dplyr::mutate(
      check_orig = sum(chala, chalb, chal_unid),
      check_indiv = sum(chal_scen1_indiv_lep, chal_scen1_indiv_cal),
      check_year = sum(chal_scen1_year_lep, chal_scen1_year_cal)
    ) %>% 
    # now make a column to see if they're all the same
    dplyr::mutate(
      all_checked = ifelse(
        check_orig == check_indiv & check_orig == check_year,
        TRUE,
        FALSE
      )
    )
  # double check the check
  if(!all(temp$all_checked)) {
    stop("ERROR - Sums not adding up")
  }
  
  set.seed(NULL)
  return(df)

}

#############################
# save_data_chalimus_drawn() function
#############################
save_data_chalimus_drawn = function(df, path) {
  
  #' Save the finished dataframe for further use
  
  readr::write_csv(df, path)
}

#############################
# get_chalimus_counts() function
#############################
get_chalimus_counts = function(df, path) {
  
  #' Use helper functions to get estimates of the number of chalimus
  
  # make average proportions for chalimus based on a mean of the motile and cope
  # proportions
  df = get_averaged_proportions(df) 
  
  # now actually draw for each of the chalimus fish
  df = count_chalimus(df)
  
  # write out the result
  save_data_chalimus_drawn(df, path)
  
  return(df)
  
}
