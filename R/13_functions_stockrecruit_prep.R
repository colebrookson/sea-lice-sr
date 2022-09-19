########## 
##########
# Get NuSEDs data in the right form
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-09-16
##########
##########

# library(here)
# library(tidyverse)
# 
# nuseds_raw = read_csv(here::here(
#   "./data/sr-data/NuSEDS/NuSEDS_20220902.csv"),
#   guess_max = 1000000)
# pink_exp = read_csv(here::here(
#   "./data/sr-data/dfo-data/raw/pink/english-report-translated.csv"))
# pink_recon = read_csv(here::here(
#   "./data/sr-data/dfo-data/clean/pink-reconstructions.csv"))
# pink_helper = read_csv(here::here(
#   "./data/sr-data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))
# 
# nuseds = trim_nuseds(nuseds_raw)
# #esc_df = pull_escapment_values(nuseds)
# rivers_helper_df = make_rivers_helper(pink_helper)
# pink_area12 = set_up_catch_data(pink_exp, pink_helper)
# esc_df_short = add_exploitation_rates(esc_df, pink_exp, pink_area12, rivers_helper_df)
# new_esc_df = set_up_full_sr_database(esc_df_short)

#############################
# get_data_nuseds_raw() function
#############################
get_data_nuseds_ref = function(file) {
  
  #' Read in .csv file with DFO farm reference numbers
  
  readr::read_csv(file, show_col_types = FALSE,
                  guess_max = 1000000)
}

#############################
# get_pink_df() function
#############################
get_pink_df = function(file) {
  
  #' Read in .csv file with DFO farm reference numbers
  
  readr::read_csv(file, show_col_types = FALSE)
}

#############################
# trim_nuseds() function
#############################
trim_nuseds = function(nuseds_raw_df) {
  
  #' Take in raw file and pass out a smaller version 
  
  nuseds = data.frame(nuseds_raw_df) %>% 
    dplyr::select(
      AREA, WATERBODY, POPULATION, RUN_TYPE, WATERSHED_CDE,
        SPECIES, ANALYSIS_YR, MAX_ESTIMATE, ADULT_PRESENCE
    ) %>% 
    dplyr::filter(SPECIES == "Pink") %>% 
    dplyr::filter(AREA %in% c("7", "8", "9", "10", "12"))
  
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
set_up_catch_data = function(pink_exp, pink_recon, pink_helper) {
  
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
  
  # now add in area 12 data to the pink_exp 
  area12_cus = c("Southern Fjords (even)", "Southern Fjords (odd)",
                 "Homathko-Klinaklini (odd)", "Nahwitti", 
                 "East Vancouver Island (odd)")
  pink_area12 = pink_recon_rate %>% 
    dplyr::filter(conservation_unit %in% area12_cus)
  
  return(pink_area12)
}

#############################
# add_exploitation_rates() function
#############################
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
    # print(row)
  }
  
  return(esc_df_short)
}

#############################
# execute_sr_data_prep() function
#############################
execute_sr_data_prep = function(raw_nuseds_raw, raw_pink_exp, 
                                raw_pink_recon, raw_pink_helper) {
  
  #' prepare the stock-recruit data before actually constructing the database
  
  # read in the data
  nuseds_raw_df = get_data_nuseds_ref(raw_nuseds_raw)
  pink_exp_df = get_pink_df(raw_pink_exp)
  pink_recon_df = get_pink_df(raw_pink_recon)
  pink_helper_df = get_pink_df(raw_pink_helper)
  
  # trim nuseds to size 
  nuseds = trim_nuseds(nuseds_raw_df)
  
  # pull escapement values 
  esc_df = pull_escapment_values(nuseds)
  
  # make helper dataframe 
  rivers_helper_df = make_rivers_helper(pink_helper_df)
  
  # separate out area 12 data 
  pink_area12 = set_up_catch_data(pink_exp_df, pink_recon_df, pink_helper_df)
  
  # add exploitation rates 
  esc_df_short = add_exploitation_rates(esc_df, pink_exp_df, pink_area12, 
                                        rivers_helper_df)
  
  return(esc_df_short)
}

#############################
# set_up_full_sr_database() function
#############################
set_up_full_sr_database = function(esc_df_short) {
  
  #' With all the information prepped, set up the full database
  
  # first check structure 
  esc_df_short$esc = as.numeric(esc_df_short$esc)
  esc_df_short[which(esc_df_short$esc == 0),"esc"] = NA
  
  # Recruitment estimates R = N/(1-u)
  esc_df_short = esc_df_short %>% 
    rowwise() %>% 
    mutate(R = esc/(1-exp))
  
  esc_df_short$S = NA
  esc_df_short$survival = NA
  
  # Spawner estimates to pair with recruitment (S(t-2) corresponds to R(t))
  esc_df_short$year = as.numeric(esc_df_short$year)
  for(river in unique(esc_df_short$river)) {
    for(year in 1954:2016) {
      
      # check if the previous year is even there 
      if(nrow(esc_df_short[which(esc_df_short$river == river &
                                 esc_df_short$year == (year - 2)),]) == 0){
        esc_df_short[which(esc_df_short$river == river & 
                             esc_df_short$year == year), "S"] = NA
        esc_df_short[which(esc_df_short$river == river & 
                             esc_df_short$year == year), "survival"] = NA
      } else {
        
        # assign the spawners
        esc_df_short[which(esc_df_short$river == river & 
                             esc_df_short$year == year), "S"] =
          esc_df_short[which(esc_df_short$river == river &
                               esc_df_short$year == (year - 2)), "esc"]
        # now assign survival 
        esc_df_short[which(esc_df_short$river == river & 
                             esc_df_short$year == year), "survival"] =
          #log(R(t)/S(t-2))
          (esc_df_short[which(esc_df_short$river == river & 
                                esc_df_short$year == year),"R"]) / 
          (esc_df_short[which(esc_df_short$river == river &
                                esc_df_short$year == year), "S"]) 
      }
    }
  }
  
  # add log survival 
  esc_df_short$log_survival = log(esc_df_short$survival)
  
  # get the rivers to be included -- these come from Steph's work
  area12_rivers = c("AHNUHATI RIVER", "AHTA RIVER", "GLENDALE CREEK", 
                    "KAKWEIKEN RIVER", "KINGCOME RIVER", "LULL CREEK", 
                    "VINER SOUND CREEK", "WAKEMAN RIVER")
  area07_rivers = c("PINE RIVER", "NEEKAS CREEK", "TANKEEAH RIVER", 
                    "KWAKUSDIS RIVER", "BULLOCK CHANNEL CREEKS", "QUARTCHA CREEK", 
                    "LEE CREEK", "ROSCOE CREEK", "CLATSE CREEK", 
                    "WALKER LAKE CREEK", "GOAT BUSHU CREEK", 
                    "DEER PASS LAGOON CREEKS", "KUNSOOT RIVER", "KADJUSDIS RIVER", 
                    "MCLOUGHLIN CREEK", "COOPER INLET CREEKS")
  
  # subset the dataframes 
  area_12_df = esc_df_short[which(esc_df_short$area == 12),]
  area_07_df = esc_df_short[which(esc_df_short$area == 7),]
  
  # all other areas 
  other_areas_df = esc_df_short[which(esc_df_short$area %notin% c(12, 7)),]
  
  # filter to only the desired areas 
  area_12_df = area_12_df %>% 
    dplyr::filter(river %in% area12_rivers)
  area_07_df = area_07_df %>% 
    dplyr::filter(river %in% area07_rivers)
  
  # bind all other areas and the filtered ones
  new_esc_df = rbind(area_07_df, area_12_df, other_areas_df)
  
  new_esc_df = new_esc_df %>% 
    rowwise() %>% 
    dplyr::mutate(even_odd = ifelse(
      (year %% 2) == 0, "even",
      "odd"
    ))

  return(new_esc_df)
}

#############################
# define_min_pairs() function
#############################
define_min_pairs = function(new_esc_df, min_pop) {
  
  #' Take the user-defined number of populations and return a dataframe with 
  #' that number of spawner-recruit pairs per population
  
  # first define populations as even/odd in the same river 
  new_esc_df$pop = NA
  num_rivers = unique(new_esc_df$river)
  
  # iterate through rivers 
  n_pops = 1
  for(river in num_rivers) {
    
    # assign population the value for odd years
    new_esc_df$pop[which(new_esc_df$river == river 
                         & new_esc_df$even_odd == "odd")] = n_pops
    # iterate
    n_pops = n_pops + 1
    # now check even years 
    new_esc_df$pop[which(new_esc_df$river == river 
                         & new_esc_df$even_odd == "even")] = n_pops
    # iterate
    n_pops = n_pops + 1
  }
  
  # make this into a factor 
  new_esc_df$pop = as.factor(new_esc_df$pop)
  
  # loop through and figure out how many pairs there are in each populations
  populations = sort(unique(new_esc_df$pop))
  counts = numeric(length(unique(new_esc_df$pop)))
  for(curr_pop in 1:length(unique(new_esc_df$pop))) {
    
    # grab the current df of the population we want 
    temp = new_esc_df[which(new_esc_df$pop == curr_pop),]
    
    # if there are enough mon-NA's then keep it
    # (I know this is ugly and slow i just wanted to make 100% sure i was doing
    # it out properly)
    n_rows = nrow(temp)
    n_NAs = nrow(temp[which(is.na(temp$survival)),])
    counts[curr_pop] = n_rows - n_NAs
    
  }
  
  pop_count_df = data.frame(
    population = populations,
    count = counts
  )
  
  # set the minimum number of populations 
  enough_obs_df = pop_count_df[which(pop_count_df$count > min_pop),]
  
  # keep only the pop's with that number
  final_rivers_df = new_esc_df[which(new_esc_df$pop %in% 
                                       enough_obs_df$population),]
  
  # name the populations
  final_rivers_df$population_name = paste(
    stringr::str_to_lower(gsub(" ", "_", final_rivers_df$river)),
    final_rivers_df$even_odd,
    sep = "_"
  )
  
  # remove NA observations of survival
  final_rivers_df = final_rivers_df %>% 
    dplyr::filter(
      !is.na(survival)
    )
  
  cat("Assuming ", min_pop, " stock-recruit pairs per population, in the 
      Final dataset: \n Total number of populations (even/odd): ", 
      length(unique(final_rivers_df$pop)), "\n Total number of S-R pairs: ", 
      dim(final_rivers_df)[1], "\n Total number of rivers: ", 
      length(unique(final_rivers_df$river)))
  
  readr::write_csv(final_rivers_df, paste0(file_path,
                                           "stock-recruit-df-no-lice-",
                                           min_pop, "-pairs.csv"))
  
  return(final_rivers_df)

}

#############################
# add_louse_covariate() function
#############################
add_louse_covariate = function(final_rivers_df, lice_pred, file_path, min_pop) {
  
  #' add in the covariate of the wild lice to this focal dataframe 
  
  final_rivers_df$lice = NA
  
  # deal with all zero values first - non-area 12, and pre-2001
  final_rivers_df[which(final_rivers_df$area != 12), "lice"] = 0
  final_rivers_df[which(final_rivers_df$area == 12 & 
                          final_rivers_df$year < 2002), "lice"] = 0
  
  # now do the area 12 that we can
  for(yr in 2002:2017) {
    
    # get the subset 
    final_rivers_df[which(final_rivers_df$area == 12 & 
                            final_rivers_df$year == yr), "lice"] = 
      # find the value from the other dataset
      lice_pred[which(lice_pred$year == yr-1), "all_lep"]
    ## NOTE ###
    # the -1 in line above is supposed to be there, to pair the year of the lice
    # infection with the return year
    ## END NOTE ##
  }
  
  readr::write_csv(final_rivers_df, past0(file_path, 
                                          "stock-recruit-data-lice-included-",
                                          min_pop, "-pairs.csv"))
  
  return(final_rivers_df)
}

#############################
# make_plot_df() function
#############################
make_plot_df = function(final_rivers_df) {
  
  #' Take dataframe and make a dataframe to plot with 
  
  # make dataframe of number of rivers sampled per year/year through time
  final_rivers_plot_df = data.frame(
    table(final_rivers_df[,c("year", "area")])
  )
  final_rivers_plot_df$year_num = 
    as.numeric(as.character(final_rivers_plot_df$year))
  final_rivers_plot_df = final_rivers_plot_df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(`Even/Odd Year` = 
                    ifelse(year_num %% 2 == 0, "even", "odd")) %>% 
    rename(Area = area)
  # check the length is correct
  if(
    nrow(final_rivers_plot_df) != length(unique(final_rivers_df$year)) * 
    length(unique(final_rivers_df$area))
  ) {
    stop("ERROR - table of incorect length")
  }
  
  return(final_rivers_plot_df)
}

#############################
# plot_df() function
#############################
plot_df = function(final_rivers_plot_df) {
  
  #' Make and return plot of the option with this many of spawner-recruit pairs

  # 20 cutoff plot
  final_rivers_plot = ggplot2::ggplot(data = final_rivers_plot_df) +
    geom_line(aes(x = year_num, y = Freq, colour = Area,
                  linetype = `Even/Odd Year`, alpha = `Even/Odd Year`), 
              size = 1.5) + 
    geom_point(aes(x = year_num, y = Freq, fill = Area,
                   shape = `Even/Odd Year`), size = 3) +
    scale_alpha_manual(values = c(0.5, 0.4)) +
    scale_shape_manual(values = c(21,22)) +
    ggthemes::theme_base() +
    theme(
      legend.key.width = unit(1.5, "cm")
    ) +
    labs(
      x = "Year", y = "Number of Rivers Surveyed"
    ) +
    scale_colour_manual(
      "Area",
      values = col_vals,
      labels = c(7, 8, 9, 10, 12)
    ) +
    scale_fill_manual(
      "Area",
      values = col_vals,
      labels = c(7, 8, 9, 10, 12)
    ) +
    annotate(
      geom = "text", x = 1998, y = 32, label = "Cutoff >= 20 S-R pairs", size = 6
    ) + 
    ylim(c(0,35)) +
    scale_x_continuous(
      labels = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
      breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
    ) +
    guides(
      fill = guide_legend(override.aes = list(shape = 21)),
      shape = guide_legend(override.aes = list(fill = "grey20"))
    ) +
    coord_fixed(2) +
    theme(
      legend.position = "none"
    )
  
  # save plot
  ggplot2::ggsave(
    paste0(fig_path, 
           "rivers-surveyed-through-time-", min_pop, "-pair-cutoff.png"),
    final_rivers_plot,
    height = 6, width = 10)
}
