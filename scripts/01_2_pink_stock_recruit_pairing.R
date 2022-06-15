##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

nuseds_raw = read_csv(here::here("./data/NuSEDS/NuSEDS_20220309.csv"))

# trim nuseds data to useful size ==============================================
names(nuseds_raw)

names_keep = c("AREA", "WATERBODY", "POPULATION", "RUN_TYPE", "WATERSHED_CDE",
               "SPECIES", "ANALYSIS_YR", "MAX_ESTIMATE", "ADULT_PRESENCE")
nuseds = nuseds_raw %>% 
  select(all_of(names_keep)) %>% 
  filter(SPECIES == "Pink") %>% 
  filter(AREA %in% c("7", "8", "9", "10", "12"))

## get escapement by either taking the value from the year and river
## or summing the values (ensure to remove NA)
## also get the area from the river 

# pull escapement data from database ===========================================

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

# set empty vectors for the other variables we will want 
esc_df = data.frame(
  years = as.factor(years), 
  rivers = as.factor(rivers), 
  escape = numeric(length(years)),
  area = numeric(length(years))
)

# empty escapment vector to put values in 
esc_vec = numeric(nrow(esc_df))
yr_vec = numeric(nrow(esc_df))
riv_vec = character(nrow(esc_df))

# loop through all the year-river combos and get escapement values 
iter = 1
for(year in 1954:2017) {
  for(river in unique(nuseds$WATERBODY)) {
    # get the subset of the data we're interested in 
    temp = nuseds[which(nuseds$ANALYSIS_YR == year & 
                          nuseds$WATERBODY == river), ]
    # find number of rows we need to deal with 
    n_obs = nrow(temp)
    if(n_obs <= 1) { # deal with cases of only one or less observations
      esc = 
        dplyr::case_when(
          n_obs == 0 ~ NA,
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
              temp$ADULT_PRESENCE %in% c("NOT INSPECTED", "UNKOWN") ~ vals[obs],
              temp$ADULT_PRESENCE == "NONE OBSERVED" ~ 0
            )
        } else if(!is.na(vals[obs])) {
          vals[obs] = vals[obs]
        }
      }
    }
    esc = sum(vals, na.rm = TRUE)
    # now put each 
    esc_vec[iter] = esc
    yr_vec[iter] = year
    riv_vec[iter] = river
    # iterate
    iter = iter + 1
  }
}





      
    )
    esc = 
      dplyr::case_when(
        n_obs == 0 ~ NA,
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
          !is.na(temp$MAX_ESTIMATE) ~ temp$MAX_ESTIMATE),  
        n_obs > 1 ~ 
          dplyr::case_when(
            # if there is a real value then just take the sum 
            if(
              
            )
          )
        (n_obs == 1 & 
           is.na(temp$MAX_ESTIMATE) & 
           temp$ADULT_PRESENCE == "NONE OBSERVED") ~ 0,
        (n_obs == 1 )
      )
    
    if (n_obs == 0) { ## CASE 1 --- NO VALUE ##
      esc = NA
    } else if(n_obs == 1) { ## CASE 2 --- ONLY ONE VALUE ##
      if(is.na(temp$MAX_ESTIMATE)) { ## CASE 2.1 --- NA IS THE VALUE ##
        if(temp$ADULT_PRESENCE %in% c("NOT INSPECTED", "UNKOWN")) {  ## CASE 2.1.1 --- NA IS THE REAL VALUE ##
          esc = temp$MAX_ESTIMATE
        } else if(temp$ADULT_PRESENCE == "NONE OBSERVED") { ## CASE 2.1.2 --- SHOULD BE ZERO-VALUED ##
          esc = 0 
        } else { # stop case 
          stop(paste0("ERROR - ", year, " ", river, " has issue"))
        } 
      } else if(!is.na(temp$MAX_ESTIMATE)) { ## CASE 2.2 --- REAL VALUE ##
        esc = temp$MAX_ESTIMATE
      } else {
        stop(paste0("ERROR - ", year, " ", river, " has issue"))
      }
    
    ## CASE 3 --- MULTIPLE VALUES ##
    
    # Since the sum of a real number and 0 or a real number and NA is the same
    # we don't need to the do the check from above
    
    else if(n_obs > 1) {
      
      # check that at least one value is real 
      temp_vec = temp$MAX_ESTIMATE
      
      # loop through the observations
      for(obs in 1:length(temp_vec)) {
        
        # check if the value is NA
        if(is.na(temp_vec[obs])) {
          
          # keep the value if it's a proper NA
          if(temp$ADULT_PRESENCE[obs] %in% c("NOT INSPECTED", "UNKOWN")) {
            temp_vec[obs] = temp$MAX_ESTIMATE[obs]
          }
          
          # if not a proper value make it zero
          else if(temp$ADULT_PRESENCE[obs] == "NONE OBSERVED") {
            temp_vec[obs] = 0 
          }
        }
        
        # if the value is NOT NA
        else if(!is.na(temp_vec[obs])) {
          temp_vec[obs] = temp$MAX_ESTIMATE[obs] 
        }
      }
      
      # now sum across the vector 
      esc = sum(na.rm = )
    }
    
  }
}

## TEST
year = unique(esc_df$years)[1]
river = unique(esc_df$rivers)[1]

year = 2001
river = "SHUSHARTIE RIVER"
for(year in unique(esc_df$years)) {
  for(river in unique(esc_df$rivers)){
    
    if(
      nrow(nuseds[which(nuseds$ANALYSIS_YR == year &
                        nuseds$WATERBODY == river),]) > 1
    ) {
      print(year); print(river)
   }
    
  }
}

## get exploitation rates from two sources of catch data 
## the pink reconstructions and the PSF data 

## Set up overall S-R data base with all rivers 

## Remove ambigious farm exposure/enhancement/etc

## make sure odd and even year populations are specified

## Only keep pop'ns with a minimum of 20 spawner-recruit pairs

## Include lice co-variate

## make final data base 


