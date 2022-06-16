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
esc_vec = rep(NA, nrow(esc_df))
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
              temp$ADULT_PRESENCE[obs] %in% c("NOT INSPECTED", "UNKOWN") ~ vals[obs],
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

# current problem is that the vectors resulting are too big - not clear why the iterator is going to high 
# but it's likely because of the fact it gets iterated in two places 


## TESTING
# year = 1954
# river = "KENNETH RIVER"
# iter = 18


## TEST
# year = unique(esc_df$years)[1]
# river = unique(esc_df$rivers)[1]
# 
# year = 2001
# river = "SHUSHARTIE RIVER"
# for(year in unique(esc_df$years)) {
#   for(river in unique(esc_df$rivers)){
#     
#     if(
#       nrow(nuseds[which(nuseds$ANALYSIS_YR == year &
#                         nuseds$WATERBODY == river),]) > 1
#     ) {
#       print(year); print(river)
#    }
#     
#   }
# }

## get exploitation rates from two sources of catch data 
## the pink reconstructions and the PSF data 

## Set up overall S-R data base with all rivers 

## Remove ambigious farm exposure/enhancement/etc

## make sure odd and even year populations are specified

## Only keep pop'ns with a minimum of 20 spawner-recruit pairs

## Include lice co-variate

## make final data base 


