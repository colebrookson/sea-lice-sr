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

df = read_csv(here("./data/wild-lice-data/clean/scfs-data-clean.csv"))
mot_ob = readRDS(here("./outputs/model-outputs/mot-regression/motile_regression_full_analysis_object.rds"))
cope_ob = readRDS(here("./outputs/model-outputs/cope-regression/cope_regression_full_analysis_object.rds"))
non_ob = readRDS(here("./outputs/model-outputs/nonlinear-regression/nonlinear_regression_full_analysis_object.rds"))
beta_ob = readRDS(here("./outputs/model-outputs/beta-regression/beta_regression_full_analysis_object.rds"))

names(df)

#############################
# get_all_model_formulations() function
#############################
get_all_motile_formulations = function(df, mot_ob, non_ob, beta_ob) {
  
  #' Use the clean scfs data and the motile logistic regression to set up the 
  #' proportions going into the Bernoulli draw
  
  # make a dataframe of yearly averages to reference later on 
  yearly_avg = df %>% 
    dplyr::select(year, prop_lep_mot) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(avg_prop = mean(prop_lep_mot, na.rm = TRUE))
  # replace the 2001 value with the modeled value
  yearly_avg$avg_prop[which(yearly_avg$year == 2001)] = 
    mean(pred_2001$pred_prop)
  
  # dataframe of the yearly averages from the nonlinear model 
  yearly_nonlinear_avg = rbind(
    # the predicted values
    cbind(
      year = 2001,
      prop_lep = stats::predict(
        # model object
        non_ob[[2]][[1]],
        # predicted dataframe
        data.frame(
          mean_all = 3.44))),
    # the model values
    non_ob[[1]] %>% 
    dplyr::select(year, prop_lep)  %>% 
      dplyr::mutate(year = as.integer(as.factor(year)))
  )
  
  # dataframe of the yearly averages from the nonlinear model 
  yearly_beta_avg = rbind(
    # the predicted values
    cbind(
      year = 2001,
      prop_lep = stats::predict(
        # model object
        beta_ob[[2]][[1]],
        # predicted dataframe
        data.frame(
          mean_all = 3.44))),
    # the model values
    non_ob[[1]] %>% 
      dplyr::select(year, prop_lep) %>% 
      dplyr::mutate(year = as.integer(as.factor(year)))
  )
    
  # join the df to the predicted proportions from the model and then add
  # in the empirical version if there is one 
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

get_all_cope_formulations = function(df, cope_op) {
  
  #' Take the cope model and add the proprotions for both the year and the 
  #' individual level assumptions to the dataframe
  
  # re-predict the model across the new all_cope (which now includes the 
  # unidentified copes)
  pred_prop_cope_indiv_scen1_df = cbind(
    obs_id = cope_ob[[4]]$obs_id,
    year = cope_ob[[4]]$year,
    data.frame(
    pred_prop_cope_indiv_scen1 = stats::predict(
      cope_ob[[2]][[1]],
      data.frame(
        all_cope = cope_ob[[4]]$unid_cope)
    )))
  
  # make yearly average
  pred_prop_cope_year_scen1_df = 
    pred_prop_cope_indiv_scen1_df %>% 
    dplyr::group_by(year) %>% 
    summarize(
      pred_prop_cope_year_scen1 = mean(pred_prop_cope_indiv_scen1)
    )
  
  df %>% 
    # add in the individual level predictions for 2002-2004 via the model 
    # predictions note that these are all given by the number of all copes 
    # available
    dplyr::left_join(.,
                     # keep only the obs_id and the pred_prop to join
                     pred_prop_cope_indiv_scen1_df %>% 
                       dplyr::select(-year),
                     by = "obs_id") %>% 
    # add in the year level predictions for 2002-2004 via the model predictions
    dplyr::left_join(.,
                     pred_prop_cope_year_scen1_df,
                     by = "year")
    
}



calculate_new_leps = function(df) {
  
  #' Using the proportions from the different model options, make estimates 
  #' of the new number of leps who are both 
}




# there must be a way to vectorize this all in a dplyr::mutate() but
# I don't know how, so here it is in a big ugly loop ---- fill in the 
# proportions of leps intelligently
for(row in seq_len(nrow(df))) {
  
  # check if the value already exists
  if(!is.na(df$pred_prop[row])) {
    next
  }
  # if in 2001 and option is individual, each line gets a prediction
  if(df$year[row] == 2001 & option == "Individual") {
    
    # get a model prediction
    assign_val = stats::predict(
      # model object
      mot_ob[[2]][[1]],
      # predicted dataframe
      data.frame(
        all_mot = df$unid_adult[row]))[[1]]
    
    # if 2001 and the option is year
  } else if(df$year[row] == 2001 & option == "Year") {
    
    # use the yearly average
    assign_val = yearly_avg$avg_prop[which(yearly_avg$year == df$year[row])]
    
    # if not 2001, check the option, if option is defer to individual
  } else if(df$unid_adult[row] > 0 & option == "Individual") {
    
    # get a model prediction
    assign_val = stats::predict(
      # model object
      mot_ob[[2]][[1]],
      # predicted dataframe
      data.frame(
        all_mot = df$unid_adult[row]))[[1]]
    
    # if the option is to defer to the year
  } else if(df$unid_adult[row] > 0 & option == "Year") {
    
    # use the yearly average
    assign_val = yearly_avg$avg_prop[which(yearly_avg$year == df$year[row])]
    
    # if the unid_adult is !> 0
  } else if(df$unid_adult[row] < 1) {
    
    # use the yearly average
    assign_val = yearly_avg$avg_prop[which(yearly_avg$year == df$year[row])]
    
    # if all else fails
  } else {
    assign_val = 0
  }
  
  # now actually use the assign_val in the right row
  df$pred_prop[row] = assign_val
  
  # at that row, determine how many unid_adults there are, and draw a sample
  # for each of them, determining if they should be added to the new lep
  # column 
  if(df$unid_adult[row] > 0) {
    for(louse in 1:df$unid_adult[row]) {
      # draw from sample
      draw = sample(c(1, 0),
                    size = 1,
                    # draw with probability of that line in the dataset for leps
                    # and 1 - that probability for cals (cals would be 0)
                    prob = c(
                      df$pred_prop[row],
                      1 - (df$pred_prop[row]))
      )
      if(draw == 1) {
        df$new_lep_adult[row] = df$new_lep_adult[row] + 1
      } else if(draw == 0) {
        df$new_cal_adult[row] = df$new_cal_adult[row] + 1
      }
    }
  }
}


