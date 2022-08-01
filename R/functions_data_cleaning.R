########## 
##########
# All functions related to cleaning the separate data items related to this 
# analysis 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# global functions =============================================================

`%notin%` = Negate(`%in%`)

standardize_names = function(df) {
  
  #' Removes any capital letters, ".", or anytime where cals or leps are not 
  #' referred to in the preferred way
  
  # get current set of names
  current_names = names(df)
  
  # loop through, pull the name out, change "." to "_"
  for(name in seq_len(length(current_names))) {
    current_names[name] = gsub("\\.", "_", current_names[name])
  }
  
  # check for any upper case letters and make those lower_case
  current_names = tolower(current_names)
  
  # standardize reference to cals or leps 
  for(name in seq_len(length(current_names))) {
    current_names[name] = gsub("caligus", "cal", current_names[name])
    current_names[name] = gsub("cals", "cal", current_names[name])
    current_names[name] = gsub("leps", "lep", current_names[name])
  }
  
  # rename the dataframe
  names(df) = current_names
  
  #return dataframe renamed 
  return(df)
}

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

get_dfo_ref_data = function(file) {
  
  #' Read in .csv file with DFO farm reference numbers

  readr::read_csv(file)
}

# marty data functions =========================================================

get_marty_data = function(file, sheet_num) {
  
  #' Read in the Marty (2010 - PNAS) dataset, noting which sheet is of use here
  
  readxl::read_excel(file, sheet = sheet_num) %>% 
   as_tibble() 
}

trim_marty_data = function(df) {
  
  #' Rename the set of column names and only keep the columns that are of use

  df %>% 
    # keep only columns of use
    dplyr::select(
      `#`, `Farm # on  Map`, `Month`, `Year`, `# fish`, `Chalimus/ fish`,
      `Motile L.s./ fish`, `Female L.s./ fish`, `Caligus/ fish`
    ) %>% 
    # get rid of the bottom rows that are just empty white space
    dplyr::slice(-(2507:nrow(df))) %>% 
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
        farm_df %>% dplyr::select(ref, name),
        # by argument for inside left_join
        by = c("farm_name" = "name")
      ),
    by = "farm_num"
  ) %>% 
    # get rid of NA_12 and NA_14 since they don't have any data 
    dplyr::filter(farm_name != c("NA_12", "NA_14"))
}

write_marty_data = function(df, file) {
  
  #' Write out final cleaned file for the Marty (2010 - PNAS) data
  
  readr::write_csv(df, file)
}

clean_marty_data = function(raw_file, sheet_number, dfo_path, output_path) {
  
  #' Compile helper functions above together to take the raw excel sheet from 
  #' Marty et al. (2010 - PNAS) and turn it into a cleaned .csv file ready 
  #' to be used in further analysis 
  
  # read in raw excel 
  raw_data = get_marty_data(raw_file, sheet_number)
  
  # read in dfo data 
  dfo_data = get_dfo_ref_data(dfo_path)
  
  # trim out unneeded columns and rename columns
  trimmed_data = trim_marty_data(raw_data)
  
  # fix the farm names
  named_data = farm_names_marty(trimmed_data, dfo_data)
  
  # fix the months so they can match up later
  months_data = fix_months(named_data) 
  
  # write out cleaned version of file
  write_marty_data(months_data, output_path)
}


clean_marty_data(
  raw_file = here::here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
  sheet_number = 2, 
  dfo_path = here::here("./data/farm-data/raw/farm-name-reference.csv"),
  output_path = here::here("./data/farm-data/clean/marty-data-clean.csv")
)

# bati-data specific functions =================================================

get_bati_data = function(file) {
  
  #' Read in the BATI dataset from the raw file 
  
  readr::read_csv(file)
}

##### TEST
# marty_df = get_marty_data(
#   here::here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
#   2)
# 
# marty_df = trim_marty_data(marty_df)
# 
# farm_df = read_csv(here::here("./data/farm-data/raw/farm-name-reference.csv"))
# named_marty = farm_names_marty(marty_df, farm_df)










































