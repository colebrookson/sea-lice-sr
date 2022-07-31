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

















































