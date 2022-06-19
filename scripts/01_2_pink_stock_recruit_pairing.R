##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(stringr)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

nuseds_raw = read_csv(here::here("./data/NuSEDS/NuSEDS_20220309.csv"))
pink_exp = 
  read_csv(here::here("./data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = 
  read_csv(here::here("./data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = 
  read_csv(here::here(
    "./data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))


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

# empty escapment vector to put values in 
esc_vec = rep(NA, length(years))
yr_vec = numeric(length(years))
riv_vec = character(length(years))

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

# cut down nuseds data to just area and river to join
nuseds_areariver = nuseds %>% 
  dplyr::select(AREA, WATERBODY) %>% 
  dplyr::rename(river = WATERBODY) %>% 
  unique()
esc_df = dplyr::left_join(
  x = esc_df,
  y = nuseds_areariver,
  by = "river"
  ) %>% 
  dplyr::rename(area = AREA)

# set up catch data ============================================================

# get a rate not percentage from English data
pink_exp$exp_rate = as.numeric(substr(pink_exp$exp_rate,
                           1,
                           nchar(pink_exp$exp_rate)-4))/100

# to apply exploitation as accurately as possible, put in all rivers that
# apply to each CU
rivers_helper_df = rbind(
  extract_rivers(pink_helper, "south_fjords_even"),
  extract_rivers(pink_helper, "south_fjords_odd"),
  extract_rivers(pink_helper, "east_vi"),
  extract_rivers(pink_helper, "hk"),
  extract_rivers(pink_helper, "nahwitti")
)
# make all upper case so they match
rivers_helper_df$rivers = stringr::str_to_upper(rivers_helper_df$rivers)

# get catch rate for the pink data from Pieter
pink_recon_rate = pink_recon %>% 
  rowwise() %>% 
  mutate(exp_rate = apportioned_catch/total_stock) %>% 
  select(conservation_unit, year, exp_rate, total_stock, apportioned_catch)

# now add in area 12 data to the pink_exp df
area12_cus = c("Southern Fjords (even)", "Southern Fjords (odd)",
               "Homathko-Klinaklini (odd)", "Nahwitti", 
               "East Vancouver Island (odd)")
pink_area12 = pink_recon_rate %>% 
  dplyr::filter(conservation_unit %in% area12_cus)

# add exploitation rates to data ===============================================

# do it in an ugly loop so it's readable

esc_df_short = esc_df %>% 
  filter(year <= 2016)
esc_df_short$exp = NA
for(row in seq_len(nrow(esc_df_short))){
  
  # find the river, area, and year first
  cur_area = esc_df_short[row, "area"]
  cur_year = esc_df_short[row, "year"]
  cur_river = esc_df_short[row, "river"]
  
  # if the area is 7-10 it's easy just take the one value
  if(cur_area %in% c(7:10)) {
    esc_df_short[row, "exp"] = 
      pink_exp[which(pink_exp$year == cur_year & 
                       pink_exp$area == cur_area), "exp_rate"]
  } else if(cur_area == 12) { # if it's area 12 then it's a bit harder
    
    # first check to see if the specific river is in the helper df
    if(cur_river %in% rivers_helper_df$rivers) {
      region = rivers_helper_df[which(rivers_helper_df$rivers == cur_river),
                                "cu"]
      
      # if the river is in two regions - deal with that 
      if(length(region > 1)) {
        if((as.numeric(cur_year) %% 2) != 0) {# if odd year take out even region
          region = region[which(region != "south_fjords_even")]
        }
      }
      
      # if the river is in the helper df, find the corresponding 
      # area and put that value in 
      if(region == "south_fjords_even") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Southern Fjords (even)"), 
                      "exp_rate"]
      } else if(region == "south_fjords_odd") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Southern Fjords (odd)"), 
                      "exp_rate"]
      } else if(region == "east_vi") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "East Vancouver Island (odd)"), 
                      "exp_rate"]
      } else if(region == "hk") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Homathko-Klinaklini (odd)"), 
                      "exp_rate"]
      } else if(region == "nahwitti") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Nahwitti"), 
                      "exp_rate"]
      } else {
        stop("ERROR - some non-matching on row ", row)
      }
    } else { # if the current river is NOT in the helper df
      # take the mean of the values in that year (likely just one)
      esc_df_short[row, "exp"] = 
        mean(pink_area12[which(pink_area12$year == cur_year),
                       "exp_rate"]$exp_rate, 
           na.rm = TRUE)
    }
  }
}

## Set up overall S-R data base with all rivers ================================

# first check structure 
esc_df_short$esc = as.numeric(esc_df_short$esc)

# Recruitment estimates R = N/(1-u)
esc_df_short = esc_df_short %>% 
  rowwise() %>% 
  mutate(R = esc/(1-exp))

# Spawner estimates to pair with recruitment (S(t-2) corresponds to R(t))
esc_df_short$S<-NA
esc_df_short$S[3:length(esc_df_short$S)] = 
  esc_df_short$esc[1:(length(esc_df_short$S)-2)]

## Remove ambigious farm exposure/enhancement/etc

## make sure odd and even year populations are specified

## Only keep pop'ns with a minimum of 20 spawner-recruit pairs

## Include lice co-variate

## make final data base 


