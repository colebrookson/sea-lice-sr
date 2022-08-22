########## 
##########
# All functions for cleaning and preparing data from 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-20
##########
##########

# global functions =============================================================
options(dplyr.summarise.inform = FALSE)
options(readr.show_col_types = FALSE)

#############################
# get_data_scfs() function
#############################
get_data_scfs = function(file) {
  
  #' Read in data from Salmon Coast Field Station sampling of wild lice
  
  readr::read_csv(file)
}

#############################
# coalesce_data_scfs() function
#############################
coalesce_data_scfs = function(df) {
  
  #' Take in raw dataframe from Salmon Coast Field Station data, clean it to 
  #' match data formats here, then also only keep the columns currently needed
  #' and coalesce the data so that all NA values are understood to be zeros

  df %>% 
    # remove bad characters and make names standrdized
    standardize_names() %>% 
    dplyr::rowwise() %>% 
    dplyr::select(year, unid_cope, lep_cope, cal_cope, lep_cope, chala, chalb, 
           chal_unid, lep_pamale, lep_male, lep_nongravid, lep_gravid,
           lep_pafemale, cal_mot, cal_gravid, unid_adult, unid_pa) %>% 
    # IMPORTANT - make all NA values zeros since they're true zeros 
    dplyr::mutate_all(., ~coalesce(.,0))
    
}

#############################
# summary_columns() function
#############################
summary_columns = function(df, path) {
  
  #' take cleaned dataframe and make summary columns that calculate the values
  #' needed for the different scenario testings. 
  
  df = df %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      # all motile leps
      lep_mot = sum(
        c(lep_pamale, lep_male, lep_nongravid, 
          lep_gravid, lep_pafemale, unid_pa),
        na.rm = TRUE),
      # all lice
      all_lice = sum(
        c(lep_cope, chala, chalb, chal_unid, lep_pamale, 
          lep_male, lep_nongravid, lep_gravid, lep_pafemale,
          cal_cope, cal_mot, cal_gravid, unid_cope, unid_adult, unid_pa),
        na.rm = TRUE),    
      # all motiles
      all_mot = sum(
          c(lep_pamale, lep_male, lep_nongravid, lep_gravid, lep_pafemale,
            cal_mot, cal_gravid, unid_pa),
          na.rm = TRUE),
      # all copes
      all_cope = sum(
        c(lep_cope, cal_cope),
        na.rm = TRUE),
      # all chalimus
      all_chal = sum(
        c(chala, chalb, chal_unid),
        na.rm = TRUE),
      # proportion of motiles that are leps
      prop_lep_mot = lep_mot/all_mot,
      # proportion of copepodids that are leps
      prop_lep_cope = lep_cope/all_cope
    )
  
  # save intermediate file
  readr::write_csv(
    df, 
    here::here(path)
  )
  
  return(df)
}

#############################
# clean_data_scfs() function
#############################
clean_data_scfs = function(df, path) {
  
  #' Use helper functions to read in raw data file and clean it, to prepare for 
  #' regression analysis 
  
  df_applied = df %>% 
    coalesce_data_scfs() %>% 
    summary_columns(., path)
  # add in unique identifier to make life easier later
  df_applied$obs_id = c(1:nrow(df_applied))

  return(df_applied)
}
