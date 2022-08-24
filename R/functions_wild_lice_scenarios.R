########## 
##########
# All functions related to comparing the different scenarios for estimating the
# number of lice on wild fish
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-13
##########
##########

options(dplyr.summarise.inform = FALSE)

# scenario 1 ===================================================================

#' Through the years of this survey, due to slight differences in sampling 
#' levels, there were variable numbers of lice in all stages of interest 
#' (copepodids, chalimus, and motiles) that were identified to species. For 
#' years in which copepodites were mostly speciated (i.e., 2005 - present), we 
#' estimated the number of unidentified copepodites that were actually 
#' L. salmonis using the empirical proportion of L. salmonis among the speciated 
#' copepodites. Similarly, for years in which motiles were mostly speciated 
#' (i.e., 2002 - present), we estimated the number of unidentified motiles that 
#' were actually L. salmonis using the empirical proportion of L. salmonis among 
#' the speciated motiles. 
#' 
#' For years in which copepodites (i.e., 2002 - 2004) and motiles (i.e., 2001) 
#' were counted but never speciated, we estimated the number of L. salmonis in 
#' these life stages using predicted L. salmonis proportions from simple
#' logistic regressions fit to the speciated years’ copepodite and motile data.
#' 
#' We estimated the number of L. salmonis chalimus-staged lice by applying the 
#' average of the L. salmonis proportions for copepodites and for motiles. 
#' Since copepodites were counted as chalimus in 2001, we estimated the L. 
#' salmonis proportion for chalimus in that year using only the motile L. 
#' salmonis proportion.
#' 
#' -----------
#'     YEAR     |   COPEPODITE   |   MOTILE    |
#'     2001     |   predicted    |  predicted  |
#'  2002-2004   |   predicted    |  empirical  |
#' 2005-present |   empirical    |  empirical  |
#' 
#' Within this approach, once we get our proportion, we use that proportion to 
#' draw from a Bernoulli distribution to decide if each of the unidentified 
#' motiles or copes are L. salmonis or not. We compare two ways of doing this. 
#' 
#' First, We calculated the mean proportion in a given year/stage and applied 
#' that to all individual observations via random sampling with a set 
#' probability according to our mean proportion. To illustrate this more 
#' clearly, for 2001 motiles, the average predicted portion of L. salmonis is 
#' 0.639. For each unidentified adult louse, to decide if that louse was 
#' L. salmonis, we drew from a Bernoulli distribution, where the probability 
#' of drawing a 1 (and therefore counting that louse as an L. salmonis louse) 
#' was equal to 0.639. In circumstances where there were >1 unidentified adult 
#' lice, this draw was repeated for each individual louse. This is more 
#' biologically realistic than ascribing 0.639 lice to a fish. 
#' 
#' Second, we use the predicted proportion given by the fitted model, to draw
#' our Bernoulli sample on the level of each fish. That is, for each row in the
#' data set, a different proportion is used to draw from the distribution. 
#' 
#' Comparing these two approaches is the difference between assumptions. The 
#' first option assumes that the proportion of L. salmonis on juvenile salmon 
#' is due primarily to the number of motiles overall on that juvenile. This 
#' means that in a well-mixed pop'n, the proportion changes as the number of 
#' motiles changes. The second option assumes that the proportion on juveniles
#' is primarily driven by changes through years external to the individual 
#' infection patterns of the fish. 
# ==============================================================================

# df = read_csv(here("./data/wild-lice-data/clean/scfs-data-clean.csv"))
# mot_ob = readRDS(here("./outputs/model-outputs/mot-regression/motile_regression_full_analysis_object.rds"))
# cope_ob = readRDS(here("./outputs/model-outputs/cope-regression/cope_regression_full_analysis_object.rds"))
# non_ob = readRDS(here("./outputs/model-outputs/nonlinear-regression/nonlinear_regression_full_analysis_object.rds"))
# beta_ob = readRDS(here("./outputs/model-outputs/beta-regression/beta_regression_full_analysis_object.rds"))

#############################
# make_yearly_mot_averages() function
#############################
make_yearly_mot_averages = function(df, mot_ob, non_ob, beta_ob) {
  
  #' Get yearly averages for the different models 
  # make a dataframe of yearly averages to reference later on 
  yearly_avg_scen1 = df %>% 
    dplyr::select(year, prop_lep_mot) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(avg_prop = mean(prop_lep_mot, na.rm = TRUE))
  # replace the 2001 value with the modeled value
  yearly_avg_scen1$avg_prop[which(yearly_avg_scen1$year == 2001)] = 
    mean(mot_ob[[4]]$pred_prop)
  
  # dataframe of the yearly averages from the nonlinear model 
  yearly_avg_scen2 = rbind(
    # the predicted values
    cbind(
      year = 2001,
      prop_lep = stats::predict(
        # model object
        non_ob[[2]][[1]],
        # predicted dataframe
        data.frame(
          mean_all = 3.44),
        type = "response")),
    # the model values
    non_ob[[1]] %>% 
      dplyr::select(year, prop_lep)  %>% 
      dplyr::mutate(year = as.numeric(as.character(year)))
  )
  
  # dataframe of the yearly averages from the beta model 
  yearly_avg_scen3 = rbind(
    # the predicted values
    cbind(
      year = 2001,
      prop_lep = stats::predict(
        # model object
        beta_ob[[2]][[1]],
        # predicted dataframe
        data.frame(
          mean_all = 3.44),
      type = "response")),
    # the model values
    non_ob[[1]] %>% 
      dplyr::select(year, prop_lep) %>% 
      dplyr::mutate(year = as.numeric(as.character(year)))
  )
  
  # make and return results list 
  yearly_avg_list_mot = list(yearly_avg_scen1, yearly_avg_scen2, yearly_avg_scen3)
  
  return(yearly_avg_list_mot)

}

#############################
# get_all_motile_formulations() function
#############################
get_all_motile_formulations = 
  function(df, mot_ob, non_ob, beta_ob, yearly_avg_list_mot) {
  
  #' Use the clean scfs data and the motile logistic regression to set up the 
  #' proportions going into the Bernoulli draw
  
  yearly_avg = yearly_avg_list_mot[[1]]
  yearly_nonlinear_avg = yearly_avg_list_mot[[2]]
  yearly_beta_avg = yearly_avg_list_mot[[3]]

  df = df %>% 
    # add in the individual level predictions for 2001 via the model predictions
    # note that these are all given by the number of all motiles available
    dplyr::left_join(.,
                     # keep only the obs_id and the pred_prop to join
                     mot_ob[[4]] %>% 
                       dplyr::select(obs_id, pred_prop),
                     by = "obs_id") %>% 
    dplyr::rename(
      pred_prop_mot_indiv_scen1 = pred_prop
    ) %>% 
    # now add in the individual level empirical predictions for >2001 -- these
    # are the rows that have an empirical prorportion of leps already 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      pred_prop_mot_indiv_scen1 = 
        # don't do the 2001 values since they are done above
        ifelse(year == 2001,
               # just keep the same value
               pred_prop_mot_indiv_scen1, 
               # if the empirical proportion is not NA, take that value
               ifelse(!is.na(prop_lep_mot),
                      prop_lep_mot,
                      # otherwise leave the NA for the loop 
                      NA))) %>% 
    # now mutate in some other new columns - add in the year default column,
    # this one just puts the yearly average for each row
    # left join to the yearly avg to get the yearly option for the prediction
    dplyr::left_join(.,
                     yearly_avg, 
                     by = "year") %>%
    dplyr::rename(
      pred_prop_mot_year_scen1 = avg_prop
    ) %>% 
    # add in the proportions according to the nonlinear regression model
    dplyr::left_join(.,
                     yearly_nonlinear_avg,
                     by = "year") %>% 
    dplyr::rename(
      pred_prop_mot_year_scen2 = prop_lep
    ) %>% 
    # add in proportions according to the beta regression model 
    dplyr::left_join(., 
                     yearly_beta_avg,
                     by = "year") %>% 
    dplyr::rename(
      pred_prop_mot_year_scen3 = prop_lep
    )
    
  return(df)

}

#############################
# make_yearly_cope_averages() function
#############################
make_yearly_cope_averages = function(df, cope_op) {
  
  #' Make yearly average dataframe to be used later for model formulations
  
  # get empirical estimates for the years
  yearly_avg_cope_scen1 = df %>% 
    dplyr::select(year, prop_lep_cope) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(avg_prop = mean(prop_lep_cope, na.rm = TRUE))
  
  # make individual-level estimates for all observations
  pred_prop_cope_indiv_scen1_df = cbind(
    obs_id = df$obs_id,
    year = df$year,
    all_cope = df$all_cope,
    data.frame(
      pred_prop_cope_indiv_scen1 = stats::predict(
        cope_ob[[2]][[1]],
        data.frame(
          all_cope = df$all_cope),
        type = "response"
      )))
  
  # make yearly average
  pred_prop_cope_year_scen1_df = 
    pred_prop_cope_indiv_scen1_df %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
      pred_prop_cope_year_scen1 = mean(pred_prop_cope_indiv_scen1)
    )
  
  pred_prop_cope_indiv_scen1_df = pred_prop_cope_indiv_scen1_df %>% 
    dplyr::select(-c(year, all_cope))
  
  # add in predicted values for 2001-2004
  yearly_avg_cope_scen1$avg_prop[
    which(yearly_avg_cope_scen1$year < 2005)] = 
    pred_prop_cope_year_scen1_df$pred_prop_cope_year_scen1[
      which(pred_prop_cope_year_scen1_df$year < 2005)
    ]
  
  yearly_avg_list_cope = list(pred_prop_cope_indiv_scen1_df, 
                              pred_prop_cope_year_scen1_df,
                              yearly_avg_cope_scen1)
  
  return(yearly_avg_list_cope)
}

#############################
# get_all_cope_formulations() function
#############################
get_all_cope_formulations = function(df, cope_op, yearly_avg_list_cope) {
  
  #' Take the cope model and add the proprotions for both the year and the 
  #' individual level assumptions to the dataframe
  
  pred_prop_cope_indiv_scen1_df = yearly_avg_list_cope[[1]]
  pred_prop_cope_year_scen1_df = yearly_avg_list_cope[[2]]
  
  df = df %>% 
    # add in the individual level predictions for 2002-2004 via the model 
    # predictions note that these are all given by the number of all copes 
    # available
    dplyr::left_join(.,
                     # keep only the obs_id and the pred_prop to join
                     pred_prop_cope_indiv_scen1_df,
                     by = "obs_id") %>% 
    # add in the year level predictions for 2002-2004 via the model predictions
    dplyr::left_join(.,
                     pred_prop_cope_year_scen1_df,
                     by = "year")
  
  return(df)
    
}

#############################
# add_in_empirical_proportions() function
#############################
add_in_empirical_proportions = function(df) {
  
  #' Since there are some of the columns that still have NA's because they are
  #' relying on yearly averages
  
  df = df %>% 
    # first, go through the mot_year_scen1 and replace any modeled observations
    # with the data observations if there are any 
    dplyr::mutate(
      pred_prop_mot_year_scen1 = ifelse(
        # if there is a value in prop lep mot, use it
        !is.na(prop_lep_mot),
        prop_lep_mot,
        pred_prop_mot_year_scen1
      ),
    # now add in any empirical proportions for the copes
      pred_prop_cope_indiv_scen1 = ifelse(
        # if there's a value in prop_lep_cope, use it
        !is.na(prop_lep_cope),
        prop_lep_cope,
        pred_prop_cope_indiv_scen1
      ),
      pred_prop_cope_year_scen1 = ifelse(
        # if there's a value in prop_lep_cope, use it
        !is.na(prop_lep_cope),
        prop_lep_cope,
        pred_prop_cope_year_scen1)
    )

    return(df)
  
}

#############################
# add_in_empirical_proportions() function
#############################
finish_prediction_proportions = 
  function(df, yearly_avg_list_mot, yearly_avg_list_cope, mot_ob) {
  
  #' Loop through the dataframe, and for any proportion column that has yet
  #' to be filled in for a given row, fill it in with the appropriate value,
  #' either predicting it based on the value or putting in the yearly value
  
  # first double check that the yearly columns for motiles in all three 
  # scenarios have no NA's
  if(any(is.na(df$pred_prop_mot_year_scen1) | 
         is.na(df$pred_prop_mot_year_scen2) |
         is.na(df$pred_prop_mot_year_scen3))) {
    stop("ERROR - Some NA's Introduced into scenario proportions")
  }
  
  yearly_avg_scen1 = yearly_avg_list_mot[[1]]
  yearly_avg_scen2 = yearly_avg_list_mot[[2]]
  yearly_avg_scen3 = yearly_avg_list_mot[[3]]
  
  yearly_avg_cope = yearly_avg_list_cope
  
  for(row in seq_len(nrow(df))) {
    
    # mot_indiv_scen1 
    if(is.na(df$pred_prop_mot_indiv_scen1[row])) {
      
      # if there are unid_mot values, use that to predict the proportion
      if(df$all_mot[row] > 0) {
        
        # predict with model object 
        fill_val = stats::predict(
          mot_ob[[2]][[1]],
          data.frame(
            all_mot = df$all_mot[row]
          ))[[1]]
      # if there's no unid_mot values, use yearly avg
      } else if(df$all_mot[row] < 1) {
        fill_val = yearly_avg_scen1$avg_prop[
          which(yearly_avg_scen1$year == df$year[row])]
      }
      
      # fill in value into the right row
      df$pred_prop_mot_indiv_scen1[row] = fill_val
    }
  }
  
  # double check there are no NA values 
  if(any(is.na(df$pred_prop_mot_year_scen1) | 
         is.na(df$pred_prop_mot_year_scen2) |
         is.na(df$pred_prop_mot_year_scen3) |
         is.na(df$pred_prop_mot_indiv_scen1) |
         is.na(df$pred_prop_cope_indiv_scen1) |
         is.na(df$pred_prop_cope_year_scen1))) {
    stop("ERROR - Not all scenario proportions filled!!")
  }
  
  return(df)
  
}

#############################
# calculate_new_leps() function
#############################
calculate_new_leps = function(df) {
  
  #' Using the proportions from the different model options, make estimates 
  #' of the new number of leps for both copes and mots
  #' 
  #' Note that for each row, there is:
  #' 1. mot_indiv_scen1
  #' 2. mot_year_scen1
  #' 3. mot_year_scen2
  #' 4. mot_year_scen3
  #' 5. cope_indiv_scen1
  #' 6. cope_year_scen1
  #' 
  #' Each of these will be used to draw a value wherever there are unidentified
  #' adults and/or copes, then the values for cals and leps of each copes 
  #' and motiles will be added to 
  
  # make empty columns first
  df = df %>% 
    mutate(
      mot_indiv_scen1_lep = 0,
      mot_indiv_scen1_cal = 0,
      mot_year_scen1_lep = 0,
      mot_year_scen1_cal = 0,
      mot_year_scen2_lep = 0,
      mot_year_scen2_cal = 0,
      mot_year_scen3_lep = 0,
      mot_year_scen3_cal = 0,
      cope_indiv_scen1_lep = 0,
      cope_indiv_scen1_cal = 0,
      cope_year_scen1_lep = 0,
      cope_year_scen1_cal = 0
    )
  
  # make vectors of the proportion columns and the new lice columns
  prop_cols = c("pred_prop_mot_indiv_scen1", "pred_prop_mot_year_scen1", 
                "pred_prop_mot_year_scen2", "pred_prop_mot_year_scen3", 
                "pred_prop_cope_indiv_scen1", "pred_prop_cope_year_scen1")
  new_lice_cols = c("mot_indiv_scen1_lep", "mot_year_scen1_lep", 
                    "mot_year_scen2_lep", "mot_year_scen3_lep", 
                    "cope_indiv_scen1_lep", "cope_year_scen1_lep", 
                    "mot_indiv_scen1_cal", "mot_year_scen1_cal",
                    "mot_year_scen2_cal", "mot_year_scen3_cal", 
                    "cope_indiv_scen1_cal", "cope_year_scen1_cal" )
  

  
  # loop through each row, make the sample pull and then 
  for(row in seq_len(nrow(df))) {
    
    # check if there are any unidentified mots 
    if(df$unid_adult[row] > 0) {
    
      for(louse in 1:df$unid_adult[row]) {

        # loop through the column options
        for(i in 1:length(prop_cols)) {
          draw = sample(c(1, 0),
                        size = 1,
                        # draw with probability of that line in the dataset for leps
                        # and 1 - that probability for cals (cals would be 0)
                        prob = c(
                          df[row, prop_cols[i]],
                          1 - (df[row, prop_cols[i]])))
          
          if(draw == 1) {
            df[row, new_lice_cols[i]] = df[row, new_lice_cols[i]] + 1
          } else if(draw == 0) {
            df[row, new_lice_cols[i + 6]] = df[row, new_lice_cols[i + 6]] + 1
          }
        }      
      }
    }
  }
  
  return(df)
}

#############################
# write_new_calculated_lice() function
#############################
write_new_calculated_lice = function(df, path) {
  
  
  #' Write out the result 
  
  readr::write_csv(df, path)
}

#############################
# count_unidentified_lice() function
#############################
count_unidentified_lice = function(df, mot_ob, cope_ob, non_ob, beta_ob, path) {
  
  #' Use all component functions to go through the data set, and given the 
  #' scenarios mentioned above, compute the number of unknown species
  #' adults and copes that are present. Currently I have this written as 
  #' reassigning `df` a number of times, which is bad but makes manual 
  #' testing easy :)
  
  # set up yearly averages for motiles
  yearly_avg_list_mot = make_yearly_mot_averages(df, mot_ob, non_ob, beta_ob)
  
  # formulate the different scenario options for motiles
  df = get_all_motile_formulations(df, mot_ob, non_ob, 
                                   beta_ob, yearly_avg_list_mot)
  
  # get yearly cope averages
  yearly_avg_list_cope = make_yearly_cope_averages(df, cope_op)
  
  # formulate different scenario options for copes
  df = get_all_cope_formulations(df, cope_ob, yearly_avg_list_cope)
  
  # add the empirical proportions from the dataset
  df = add_in_empirical_proportions(df)
  
  # make the prediction proportios
  df = finish_prediction_proportions(df, yearly_avg_list_mot, 
                                     yearly_avg_list_cope, mot_ob)
  
  # actually draw for the new leps
  df = calculate_new_leps(df)
  
  # write result out
  write_new_calculated_lice(df, path)
  
  return(df)
  
}


