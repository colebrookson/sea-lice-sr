########## 
##########
# Get NuSEDs data in the right form
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

library(here)
library(tidyverse)

nuseds_raw = read_csv(here::here(
  "./data/sr-data/NuSEDS/NuSEDS_20220309.csv"))
pink_exp = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = read_csv(here::here(
  "./data/sr-data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))

nuseds = trim_nuseds(nuseds_raw)

#############################
# trim_nuseds() function
#############################
trim_nuseds = function(nuseds_raw) {
  
  #' Take in raw file and pass out a smaller version 
  
  names_keep = c("AREA", "WATERBODY", "POPULATION", "RUN_TYPE", "WATERSHED_CDE",
                 "SPECIES", "ANALYSIS_YR", "MAX_ESTIMATE", "ADULT_PRESENCE")
  nuseds = nuseds_raw %>% 
    select(all_of(names_keep)) %>% 
    filter(SPECIES == "Pink") %>% 
    filter(AREA %in% c("7", "8", "9", "10", "12"))
  
  return(nuseds)
}

pull_escapment_values = function(nuseds) {
  
  #' Pull escapement values from the database in the right order 
  
  # set up a vector for years to be the column in the dataframe 
  if(length(unique(nuseds$WATERBODY)) != 279) {
    stop("ERROR: Number of Rivers Is Not Right! Should be 279")
  }
  years = rep(1954:2017, length(unique(nuseds$WATERBODY)))
  
  # same with the rivers
  rivers = rep(unique(nuseds$WATERBODY), each = length(1954:2017))
  
  if(length(years) != length(rivers)) {
    stop("ERROR: length of rivers vector and years vector are not equal")
  }
  
  # empty escapment vector to put values in 
  esc_vec = rep(NA, length(years))
  yr_vec = numeric(length(years))
  riv_vec = character(length(years))
  
  # keeping this in a loop because that's how Peacock et al. (2013) did it 
  # and it's easier to keep that consistent 
  iter = 1
  for(year in 1954:2017) {
    for(river in unique(nuseds$WATERBODY)) {
      # get the subset of the data we're interested in 
      temp = nuseds %>% 
        dplyr::filter(
          ANALYSIS_YR == year &
          WATERBODY == river
        )
      temp = nuseds[which(nuseds$ANALYSIS_YR == year & 
                            nuseds$WATERBODY == river), ]
      # find number of rows we need to deal with 
      n_obs = nrow(temp)
      if(n_obs == 0) { # do zero case here
        # essentially just keep the current value of NA
        esc = -9999999      
      } else if(n_obs == 1) { # deal with cases of only one or less observations
        esc = 
          dplyr::case_when(
            n_obs == 1 ~
              # sub-case
              dplyr::case_when(
                is.na(temp$MAX_ESTIMATE) ~
                  # sub-case when is.na is true
                  dplyr::case_when(
                    # proper NA
                    temp$ADULT_PRESENCE %in% c("NOT INSPECTED",
                                               "UNKOWN") ~ temp$MAX_ESTIMATE,
                    # should be zero
                    temp$ADULT_PRESENCE == "NONE OBSERVED" ~ 0),
                # sub case if is.na isn't true
                !is.na(temp$MAX_ESTIMATE) ~ temp$MAX_ESTIMATE)
          )
      } else { # now deal with more than one observation 
        vals = temp$MAX_ESTIMATE
        for(obs in 1:nrow(temp)) {
          if(is.na(vals[obs])) {
            vals[obs] = 
              dplyr::case_when(
                temp$ADULT_PRESENCE[obs] %in% c("NOT INSPECTED", "UNKOWN") ~ 
                  vals[obs],
                temp$ADULT_PRESENCE[obs] == "NONE OBSERVED" ~ 0
              )
          } else if(!is.na(vals[obs])) {
            vals[obs] = vals[obs]
          }
        }
        esc = sum(vals, na.rm = TRUE)
      }
      # now put each 
      esc_vec[iter] = esc
      yr_vec[iter] = year
      riv_vec[iter] = river
      # iterate
      iter = iter + 1
    }
  }
  
  # et rid of all -9999999 values
  esc_vec[which(esc_vec == -9999999)] = NA
  
  # now turn vectors into a dataframe
  esc_df = data.frame(
    cbind(
      esc = esc_vec,
      river = riv_vec,
      year = yr_vec
    )
  )
}