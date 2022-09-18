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
  "./data/sr-data/NuSEDS/NuSEDS_20220902.csv"),
  guess_max = 1000000)
pink_exp = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = read_csv(here::here(
  "./data/sr-data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = read_csv(here::here(
  "./data/sr-data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))

nuseds = trim_nuseds(nuseds_raw)
#esc_df = pull_escapment_values(nuseds)
rivers_helper_df = make_rivers_helper(pink_helper)
pink_area12 = set_up_catch_data(pink_exp, pink_helper)
esc_df_short = add_exploitation_rates(esc_df, pink_exp, pink_area12, rivers_helper_df)

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

#############################
# pull_escapment_values() function
#############################
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

  return(esc_df)  
}

#############################
# extract_rivers() function
#############################
extract_rivers = function(df, cu) {
  
  #' Helper function to find the CU and get the rivers according to that 
  #' CU and return it
  
  # isolate the values
  temp = df[which(df$cu == cu ), "rivers"]
  
  # split them up 
  temp_list = stringr::str_split(temp, ", ")
  
  # get vector from list 
  temp_vec = temp_list[[1]]
  
  # get names vec 
  names = rep(cu, length(temp_vec))
  
  # bind into dataframe
  temp_df = data.frame(rivers = temp_vec, cu = names)
  
  return(temp_df)
}

#############################
# make_rivers_helper() function
#############################
make_rivers_helper = function(pink_helper) {
  
  #' Make and return a helper dataframe with the rivers extracted 
  
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
  
  return(rivers_helper_df)
}

#############################
# set_up_catch_data() function
#############################
set_up_catch_data = function(pink_exp, pink_helper) {
  
  #' get catch data for pink salmon 
  
  # get a rate not percentage from English data
  pink_exp$exp_rate = as.numeric(substr(pink_exp$exp_rate,
                                        1,
                                        nchar(pink_exp$exp_rate)-4))/100
  
  # get catch rate for the pink data from Pieter
  pink_recon_rate = pink_recon %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      exp_rate = apportioned_catch/total_stock
      ) %>% 
    dplyr::select(
      conservation_unit, year, exp_rate, total_stock, apportioned_catch)
  
  # now add in area 12 data to the pink_exp df
  area12_cus = c("Southern Fjords (even)", "Southern Fjords (odd)",
                 "Homathko-Klinaklini (odd)", "Nahwitti", 
                 "East Vancouver Island (odd)")
  pink_area12 = pink_recon_rate %>% 
    dplyr::filter(conservation_unit %in% area12_cus)
  
  return(pink_area12)
}

add_exploitation_rates = function(esc_df, pink_exp, 
                                  pink_area12, rivers_helper_df) {
  
  #' Add in the exploitation rates from the other data source 
  
  esc_df_short = esc_df %>% 
    dplyr::filter(year <= 2016)
  esc_df_short$exp = NA
  
  # join the two pink exp dfs
  # pink_exp$conservation_unit = NA
  # rbind(pink_exp,
  #       pink_area12 %>% 
  #         dplyr::mutate(area = 12) %>% 
  #         dplyr::select(year, exp_rate, area, conservation_unit)
  #       )
  # 
  # 
  # esc_df_short %>% 
  #   dplyr::rowwise() %>% 
  #   dplyr::mutate(
  #     exp = dplyr::case_when(
  #       
  #     )
  #   )
  
  
  # again, kind of ugly loop but makes it more explicit
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
          # look at the actual options for the region 
          curr_options = pink_area12[which(
            pink_area12$year == cur_year), ]
          curr_options = curr_options %>% 
            dplyr::filter(!is.na(exp_rate))
          if(nrow(curr_options) < 1) {
            esc_df_short[row, "exp"] = NA
            next
          } else if(nrow(curr_options) == 1) {
            if(curr_options$conservation_unit == "Southern Fjords (even)") {
              region = "south_fjords_even"
            } else if(curr_options$conservation_unit == 
                      "Southern Fjords (odd)") {
              region = "south_fjords_odd"
            } 
          } else if(nrow(curr_options) == 2) {
            if((as.numeric(cur_year) %% 2) != 0) {
              # if odd year take out even region
              region = region[which(region != "south_fjords_even")]
            } 
          } else if(nrow(curr_options) > 2) {
            if(length(unique(round(curr_options$exp_rate, digits = 5))) == 1) {
              esc_df_short[row, "exp"] = curr_options[1, "exp_rate"]
              next
            }
          }
          if(length(region) > 1 & length(unique(round(curr_options$exp_rate, 
                                                      digits = 5))) > 1) 
          {
            esc_df_short[row, "exp"] = 
              mean(pink_area12[which(pink_area12$year == cur_year),
                               "exp_rate"]$exp_rate, 
                   na.rm = TRUE)
          }
        }
        if(!is.na(esc_df_short[row, "exp"])) {
          next
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
    print(row)
  }
  
  return(esc_df_short)
}


