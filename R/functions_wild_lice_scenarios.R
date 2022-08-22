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

names(df)

# get the average predicted proportion in 2001 for motiles
count_motile_2001_lice = function(df, mot_ob, option) {
  
  #' Use the clean scfs data and the motile logistic regression to set up the 
  #' proportions going into the Bernoulli draw
  
  # the fourth element of mot_ob is the predicted proportions for 2001
  pred_2001 = mot_ob[[4]] %>% 
    dplyr::select(year, obs_id, pred_prop, lep_pamale, lep_male, lep_nongravid, 
                  lep_gravid, lep_pafemale, unid_pa, unid_adult)
  
  # make a dataframe of yearly averages to reference later on 
  yearly_avg = df %>% 
    dplyr::select(year, prop_lep_mot) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(avg_prop = mean(prop_lep_mot, na.rm = TRUE))
    
  # join the df to the predicted proportions from the model and then add
  # in the empirical version if there is one 
  df = df %>% 
    dplyr::left_join(.,
                     # keep only the obs_id and the pred_prop to join
                     mot_ob[[4]] %>% 
                       dplyr::select(obs_id, pred_prop),
                     by = "obs_id") %>% 
    dplyr::rowwise() %>% 
    # add in the empirical proportions
    dplyr::mutate(
      pred_prop = 
        # if the empirical proportion is not NA, take that value
        ifelse(!is.na(prop_lep_mot),
               prop_lep_mot,
               NA)
      )
  
  # there must be a way to vectorize this all in a dplyr::mutate() but
  # I don't know how, so here it is in a big ugly loop ---- fill in the 
  # proportions of leps intelligently
  for(row in seq_len(nrow(df))) {
    
    # check if the value already exists
    if(!is.na(df$pred_prop[row])) {
      next
    }
    # if not, check the option, if option is defer to individual
    if(df$unid_adult[row] > 0 & option == "Individual") {
      
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
        assign_val = NA
    }
    
    # now actually use the assign_val in the right row
    df$pred_prop[row] = assign_val
  }
  
 
}







