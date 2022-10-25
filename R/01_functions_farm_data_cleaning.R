########## 
##########
# All functions related to cleaning the separate data items related to this 
# analysis 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-20
##########
##########

# global functions =============================================================
options(dplyr.summarise.inform = FALSE)

#############################
# fix_months() function
#############################
fix_months = function(df) {
  
  #' Takes in a dataframe with named months in string format and returns a 
  #' dataframe that has numeric months matching the named months
  
  # use a simple joined dataframe to match across 
  month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                  "Sep", "Oct", "Nov", "Dec")
  month_numbers = seq(1, 12, 1)
  
  month_df = data.frame(
    month = month_names, 
    num = month_numbers
  )
  
  # ensure all the month names in the vector above and in the df are the same
  df_names = sort(unique(df$month))
  month_sorted = sort(month_names)
  
  if (!identical(df_names, month_sorted)) {
    stop("Months in function and months in dataframe do not match!")
  }
  
  # left join the data 
  df_month_nums = dplyr::left_join(
    df, 
    month_df, 
    by = "month"
  )
  
  # keep the number of the month and get rid of the text version 
  df_months = df_month_nums %>% 
    dplyr::select(
      -month
    ) %>% 
    dplyr::rename(
      month = num
    )
  
  # return
  return(df_months)
  
}

#############################
# get_data_dfo_ref() function
#############################
get_data_dfo_ref = function(file) {
  
  #' Read in .csv file with DFO farm reference numbers

  readr::read_csv(file, show_col_types = FALSE) %>% 
    dplyr::select(ref, name)
}

# marty data functions =========================================================

#############################
# get_data_marty() function
#############################
get_data_marty = function(file) {
  
  #' Read in the Marty (2010 - PNAS) dataset, noting which sheet is of use here
  
  readr::read_csv(file, show_col_types = FALSE)
}

#############################
# trim_marty_data() function
#############################
trim_marty_data = function(df) {
  
  #' Rename the set of column names and only keep the columns that are of use

  df[,c(1,2,4,5,9:12,15)] %>% 
    # keep only columns of use
    # dplyr::select(
    #   `#`, `Farm # on  Map`, `Month`, `Year`, `# fish`, `Chalimus/ fish`,
    #   `Motile L.s./ fish`, `Female L.s./ fish`, `Caligus/ fish`
    # ) %>% 
    # rename the variables to new_names
    dplyr::rename(
      obs_num = `#`,
      farm_num = `Farm # on  Map`, 
      month = Month, 
      year = Year, 
      inventory = `# fish`, 
      chal_av = `Chalimus/ fish`, 
      lep_av_mot = `Motile L.s./ fish`, 
      lep_av_fem = `Female L.s./ fish`, 
      cal_av = `Caligus/ fish`
    )
}

#############################
# farm_names_marty() function
#############################
farm_names_marty = function(marty_df, farm_df) {
  
  #' Standardize the names of the farms in the Marty dataset
  
  dplyr::left_join(
    # the marty dataframe 
    marty_df,
    # intermediate dataframe with the DFO reference numbers
    dplyr::left_join(
      # make dataframe with the names in the order to match the numbers that
      # Marty 2010 used 
      data.frame(
        farm_name = c(
          "Simmonds Point", "Wehlis Bay", "Maude Island", "Cecil Island",
          "Cypress Harbour", "Sir Edmund Bay", "NA_7", "Cliff Bay", 
          "Glacier Falls", "Burdwood", "NA_12", "Wicklow Point", "NA_14", 
          "NA_15", "Upper Retreat", "Arrow Pass", "Midsummer", "Potts Bay", 
          "Port Elizabeth", "Humphrey Rock", "Sargeaunt Pass", "Doctor Islets", 
          "Swanson", "Larsen Island", "Noo-la"
        ), 
        farm_num = c(seq(1, 9, 1), seq(11, 26, 1))),
        # other dataframe here is the DFO farm reference numbers dataframe
        data.frame(farm_df),
        # by argument for inside left_join
        by = c("farm_name" = "name")
      ),
    by = "farm_num"
  ) %>% 
    # get rid of NA_12 and NA_14 since they don't have any data 
    dplyr::filter(farm_name %notin% c("NA_12", "NA_14")) %>% 
    dplyr::rename(farm_ref = ref)
}

#############################
# write_data_marty() function
#############################
write_data_marty = function(df, file) {
  
  #' Write out final cleaned file for the Marty (2010 - PNAS) data
  
  readr::write_csv(df, file)
}

clean_data_marty = function(raw_file, dfo_file, output_path) {
  
  #' Compile helper functions above together to take the raw excel sheet from 
  #' Marty et al. (2010 - PNAS) and turn it into a cleaned .csv file ready 
  #' to be used in further analysis 
  
  # read in raw file 
  get_data_marty(raw_file) %>%
    # trim out unneeded columns and rename columns
    trim_marty_data(.) %>%
    # fix the farm names
    farm_names_marty(., dfo_file) %>%
    # fix the months so they can match up later
    fix_months(.) %>% 
    # fix the months so they can match up later
    write_data_marty(., output_path)
}

# bati-data specific functions =================================================

#############################
# get_data_bati() function
#############################
get_data_bati = function(file) {
  
  #' Read in the BATI dataset from the raw file 
  
  readr::read_csv(file, show_col_types = FALSE)
}

#############################
# farm_names_bati() function
#############################
farm_names_bati = function(bati_df, dfo_names) {

  #' Using the DFO reference data, change small inconsistencies in the names
  #' of farms in the BATI dataset, and add in the reference number, so as to 
  #' be consistent across all files
  #' 
  #' Note that the farms in the BATI dataset that are not named according to the
  #' DFO naming convention are: "Arrow Passage", "Humphrey Rocks", 
  #' "Midsummer Island", "Sargeaunt Passage", "Swanson Island"
  
  dplyr::left_join(
    bati_df %>% 
      dplyr::mutate(farm_name = dplyr::case_when(
        farm == "Arrow Passage"     ~ "Arrow Pass",
        farm == "Humphrey Rocks"    ~ "Humphrey Rock",
        farm == "Midsummer Island"  ~ "Midsummer",
        farm == "Sargeaunt Passage" ~ "Sargeaunt Pass",
        farm == "Swanson Island"    ~ "Swanson",
        TRUE                        ~ farm
      )) %>% 
      dplyr::select(-farm),
    dfo_names %>% 
      dplyr::select(name, ref),
    by = c("farm_name" = "name")
  )
}

#############################
# write_data_bati() function
#############################
write_data_bati = function(df, file) {
  
  #' Write out final cleaned file for the BATI-provided data
  
  readr::write_csv(df, file)
}


#############################
# clean_data_bati() function
#############################
clean_data_bati = function(raw_file, dfo_path, output_path) {
  
  #' Compile helper functions that clean the BATI dataset and prepare 
  #' the raw BATI file for joining and analysis
  
  get_data_bati(raw_file) %>% 
    standardize_names(.) %>% 
    farm_names_bati(., get_data_dfo_ref(dfo_path)) %>% 
    write_data_bati(., output_path)
}

# DFO farm data cleaning functions =============================================

#############################
# get_data_marty_cleaned() function
#############################
get_data_marty_cleaned = function(file) {
  
  #' Read in cleaned and prepped Marty (2010) data
  
  readr::read_csv(file, show_col_types = FALSE)
}

#############################
# get_data_dfo_open() function
#############################
get_data_dfo_open = function(file) {
  
  #' Read in data from open DFO website on average lice counts
  
  readr::read_csv(file, show_col_types = FALSE)
}

#############################
# clean_dfo_open_data() function
#############################
clean_dfo_open_data = function(df, output_file) {
  
  #' Function to read in messy DFO data, clean it, and write it out 
  df %>% 
    # rename first for easier referencing
    dplyr::rename(
      year = Year, month = Month, farm_name = "Site Common Name", 
      lep_av = "Average L. salmonis females per fish",
      cal_av = "Average caligus per fish",
      ref = "Facility Reference Number"
    ) %>% 
    # keep relevant columns only
    dplyr::select(
      year, month, farm_name, lep_av, cal_av, ref
    ) %>%       
    # perform quick join to get months in number format
    dplyr::left_join(.,
                   data.frame(
                     month = unique(.$month),
                     month_num = seq(1,12,1)
                   ),
                   by = "month") %>% 
    dplyr::select(-month) %>% 
    dplyr::rename(month = month_num) %>% 
    readr::write_csv(., output_file)
    
}

#############################
# calculate_missing_averages() function
#############################
calculate_missing_averages = function(marty_df) {
  
  #' Using the cleaned Marty (2010) data, calculate the inventory averages
  #' during the time periods we need to fill in for dates post-2009, for farms
  #' outside of BATI control. These farms we have lice data for from open DFO
  #' data, but not inventory data, so we have to extrapolate
  
  marty_df %>% 
    # keep only farms that are in our list of 4 farms that have missing data
    dplyr::filter(
      farm_name %in% c("Maude Island", "Wehlis Bay", "Simmonds Point",
                       "Noo-la")
    ) %>% 
    # shrink df down
    dplyr::select(
      inventory, month, year, farm_name, farm_ref
    ) %>% 
    # now filter down to just the months we want
    dplyr::filter(
      month %in% c(3, 4)
    ) %>% 
    # now group by month and farm name to use summarize
    dplyr::group_by(
      month, farm_name, farm_ref
    ) %>% 
    # summarize to get a mean inventory across each month/farm combo
    dplyr::summarize(
      mean_inventory = mean(inventory, na.rm = TRUE)
    )
}

#############################
# trim_clean_dfo_open_data() function
#############################
trim_clean_dfo_open_data = function(dfo_df, marty_df) {
  
  #' Summarise from the open DFO data, the information for the four missing 
  #' farms that we want information for 
  
  dfo_df %>% 
    # keep only the farms we want
    dplyr::filter(
      farm_name %in% c("Maude Island", "Wehlis Bay", "Simmonds Point",
                       "Noo-la")
    ) %>% 
    # keep only the months we want
    dplyr::filter(
      month %in% c("March", "April")
    ) %>% 
    # get an average value for each month/year/farm combo 
    dplyr::group_by(farm_name, year, month) %>% 
    dplyr::summarize(
      lep_av = mean(lep_av, na.rm = TRUE),
      cal_av = mean(cal_av, na.rm = TRUE)
    ) %>% 
    # rename the months to numbers for consistency & make other import. columns
    dplyr::mutate(
      month = ifelse(month == "March", 3, 4),
      ktf = "Broughton", 
      hump_sarg_doc = "Other"
    ) %>% 
    # now join with the mean inventory calculations from the clean marty df
    dplyr::left_join(
      x = .,
      y = calculate_missing_averages(marty_df),
      by = c("month", "farm_name")
    ) %>% 
    # rename for consistency
    dplyr::rename(inventory = mean_inventory) %>% 
    # now calculate total caligus and l. salmonis counts 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lep_tot = inventory * lep_av,
      cal_tot = inventory * cal_av
    ) %>% 
    # sort names for easy joining
    dplyr::select(
      sort(names(.))
    )
}

#############################
# write_dfo_filled_missing_data() function
#############################
write_dfo_filled_missing_data = function(data, file) {
  
  #' Write out file of DFO/Marty (2010) paired data which fills in inventory
  #' gaps with extrapolated info 
  
  readr::write_csv(data, file)
}

#############################
# fill_in_missing_inventory_data() function
#############################
fill_in_missing_inventory_data = function(dfo_df, marty_df, output_file) {
  
  #' Use helper functions to fill in the missing inventory data for four 
  #' farms post 2009 with averages from pre-2009 for inventory data, and DFO 
  #' open data on lice averages 
  
  dfo_df %>% 
    # perform all trimming and preparing
    trim_clean_dfo_open_data(., marty_df) %>% 
    # write out result
    write_dfo_filled_missing_data(., output_file)
}

# missing inventory data for late timeseries farms functions ===================

#############################
# match_inventory_data() function
#############################
match_inventory_data = function(bati_df, dfo_df, output_path) {
  
  #' Check when we have no inventory if there is a lice measurement for that 
  #' time period 
  
  # make df with the lep mean measures for the months that there is at least 
  # one count for 
  dfo_df %>% 
    # filter both the months we want and only the farms in the BATI data
    dplyr::filter(month %in% c(3,4) &
                    ref %in% bati_df$ref
    ) %>% 
    dplyr::group_by(
      month, year, ref
    ) %>% 
    dplyr::summarize(
      lep_av = mean(lep_av, na.rm = TRUE)
    ) %>% 
    # need to ungroup to use the complete() function
    dplyr::ungroup() %>% 
    tidyr::complete(
      ., year, month, ref
    ) %>% 
    # make a column for the inventory data to come in from 
    #dplyr::mutate(inventory = vector(mode = "numeric", length = nrow(.))) %>% 
    # left join to BATI data to get inventory data
    dplyr::left_join(.,
                     bati_df %>% 
                       dplyr::select(ref, month, year, inventory, farm_name),
                     by = c("ref", "month", "year")
    ) %>% 
    unique() %>% 
    # ungroup to regroup 
    dplyr::ungroup() %>% 
    dplyr::group_by(
      ref, month
    ) %>% 
    # get one value for each farm/month combo 
    dplyr::summarize(
      inventory = mean(inventory, na.rm = TRUE)
    ) %>% 
    # keep out the broodstock farms - cecil, cyprus, and pott's bay 
    dplyr::filter(
      ref %notin% c(1145, 458, 819)
    ) %>% 
    # ungroup and group again
    dplyr::ungroup() %>% 
    # get rid of farm identifiers so it's just month and inventory
    dplyr::select(
      -ref
    ) %>% 
    dplyr::group_by(month) %>% 
    # get just two monthly averages
    dplyr::summarize(
      inventory = mean(inventory)
    ) %>% 
    # now join back to the dfo data, bringing in the other important information
    dplyr::left_join(.,
                     dfo_df %>% 
                       # Tsa-ya is 7273 and Wa-kwa is 1839
                       dplyr::filter(ref %in% c(7273, 1839) &
                                       month %in% c(3, 4)) %>% 
                       dplyr::group_by(year, ref, month, farm_name) %>% 
                       dplyr::summarize(lep_av = mean(lep_av, na.rm = TRUE)) %>% 
                       dplyr::filter(!is.nan(lep_av)),
                     by = c("month")) %>% 
    # add in other columns that are handy to have in 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lep_tot = inventory * lep_av,
    ) %>% 
    # write the result for future use
    readr::write_csv(.,
                     output_path)
}

# functions to join all farm data together =====================================

# bati_df = read_csv(here("./data/farm-data/clean/bati-data-cleaned.csv"))
# marty_df = read_csv(here("./data/farm-data/clean/marty-data-clean.csv"))
# missing_df = read_csv(here("./data/farm-data/clean/missing-inventory-filled-data.csv"))
# late_df = read_csv(here("./data/farm-data/clean/wakwa-tsaya-inventory.csv"))

#############################
# join_clean_bati() function
#############################
join_clean_bati = function(bati_df) {
  
  #' Clean BATI to prepare to join into one dataframe with other data sources
  
  bati_df %>% 
    # add data source identifier
    dplyr::mutate(
      data_source = "BATI"
      ) %>% 
    dplyr::select(
      -c(date, cal_av, cal_tot)
      ) %>% 
    dplyr::mutate(
      hump_sarg_doc = dplyr::case_when(
        farm_name == "Sargeaunt Pass" ~ 1,
        farm_name == "Doctor Islets"  ~ 1,
        farm_name == "Humphrey Rock"  ~ 1,
        TRUE                          ~ 0
      ),
      ktf = dplyr::case_when(
        ref %in% c(136, 728, 820, 821, 
                1059, 1144, 1586, 1618)   ~ 1,
        farm_name == "NA_7"               ~ 1,
        TRUE                              ~ 0
      ),
      # also make unique farm/year/month identifier
      time_place_id = paste(ref, year, month, sep = "_")
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lep_tot = inventory * lep_av
    ) %>% 
    select(sort(names(.)))
}

#############################
# join_clean_marty() function
#############################
join_clean_marty = function(marty_df) {
  
  #' Clean Marty (2010) to join into one dataframe with other data sources
  
  marty_df %>% 
    dplyr::mutate(
      data_source = "MARTY"
      ) %>% 
    dplyr::filter(
      month %in% c(3, 4)
      ) %>% 
    dplyr::select(
      -c(obs_num, farm_num, chal_av, lep_av_mot, cal_av)
      ) %>% 
    dplyr::rename(
      ref = farm_ref,
      lep_av = lep_av_fem
      ) %>% 
    dplyr::mutate(
      hump_sarg_doc = dplyr::case_when(
        farm_name == "Sargeaunt Pass" ~ 1,
        farm_name == "Doctor Islets"  ~ 1,
        farm_name == "Humphrey Rock"  ~ 1,
        TRUE                          ~ 0
      ),
      ktf = dplyr::case_when(
        ref %in% c(136, 728, 820, 821, 
                1059, 1144, 1586, 1618)   ~ 1,
        farm_name == "NA_7"               ~ 1,
        TRUE                              ~ 0
      )
    ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      lep_tot = inventory * lep_av,
      # also make unique farm/year/month identifier
      time_place_id = paste(ref, year, month, sep = "_")
    ) %>% 
    select(sort(names(.)))
}

#############################
# join_clean_missing() function
#############################
join_clean_missing = function(missing_df) {
  
  #' Clean missing data from later years for the identified farms 
  #'  to prepare to join into one dataframe with other data sources
  
  missing_df %>% 
    dplyr::mutate(
      data_source = "AVG_PREV_YRS"
      ) %>% 
    select(
      -c(cal_av, cal_tot)
      ) %>% 
    rename(
      ref = farm_ref
      ) %>% 
    mutate(
      # also make unique farm/year/month identifier
      time_place_id = paste(ref, year, month, sep = "_")
    ) %>% 
    select(sort(names(.)))
}

#############################
# join_clean_late() function
#############################
join_clean_late = function(late_df) {
  
  #' Clean data from the late farms in the timeseries to prepare to bind all 
  #' sources of farm data together
  
  late_df %>% 
    dplyr::mutate(
      data_source = "AVG_ALL_FARMS",
      ktf = 0,
      hump_sarg_doc = 0,
      # also make unique farm/year/month identifier
      time_place_id = paste(ref, year, month, sep = "_")
      ) %>% 
    select(sort(names(.)))
}

#############################
# write_data_joined_farm() function
#############################
write_data_joined_farm = function(df, output_path) {
  
  readr::write_csv(df, output_path)
}

#############################
# join_farm_data() function
#############################
join_farm_data = function(bati_df, marty_df, missing_df, late_df, output_path) {
  
  #' Function to join four data sources. BATI data, which has a good chunk of 
  #' the data we're using, Marty (2010) data, which has more information 
  #' earlier in the timeseries, and two others. Here, missing_df is 
  #' extrapolated data for 4 farms that we don't have inventory data for past
  #' 2009. In addition, the late_df is the extrapolated inventory data for 
  #' two farms that popped up late in the timeseries and that we don't have
  #' any inventory data for at all. Note that I will default to BATI data, 
  #' then Marty, then the other two can just get joined at the end
  
  df = data.frame(rbind(
    join_clean_bati(bati_df),
    # this is the deferment step
    join_clean_marty(marty_df) %>% 
      filter(time_place_id %notin% join_clean_bati(bati_df)$time_place_id),
    join_clean_missing(missing_df),
    join_clean_late(late_df)
    ))
  
  # write out the data 
  write_data_joined_farm(df, output_path)
  
  return(df)
    
  
}
