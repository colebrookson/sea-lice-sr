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

get_data_scfs = function(file) {
  
  #' Read in data from Salmon Coast Field Station sampling of wild lice
  
  readr::read_csv(file)
}

clean_scfs_data = function(df, include_chal) {
  
  #' Take in raw dataframe from Salmon Coast Field Station data, clean it to 
  #' match data formats here, then summarize the dataset to get information
  #' on the number of lice per fish per year. Optionally, include chalimus
  #' or do not include chalimus in the data counts. 
  x = scfs_df %>% 
    standardize_names() %>% 
    group_by(year) %>% 
    summarize_at(., .vars = c("lep_cope", "chala", "chalb", "chal_unid", "lep_pamale", 
                                                                     "lep_male", "lep_nongravid", "lep_gravid", "lep_pafemale",
                                                                     "cal_mot", "cal_gravid", "unid_adult", "unid_pa"),
                                        .funs = mean, na.rm = TRUE)
  x = scfs_df %>% 
    # remove bad characters and make names standrdized
    standardize_names() %>% 
    dplyr::rowwise() %>%
    dplyr::mutate( # make columns that divide the lice into species 
      all_lep = sum(
        # if statement controls if chalimus are included or not 
        if(include_chal == TRUE) {
          c(lep_cope, chala, chalb, chal_unid, lep_pamale, 
                        lep_male, lep_nongravid, lep_gravid, lep_pafemale)
        } else {
          c(lep_cope, lep_pamale, lep_male, lep_nongravid, 
                           lep_gravid, lep_pafemale)
        }
        , na.rm = TRUE),
      all_lice = sum(
        if(include_chal == TRUE) {
          c(lep_cope, chala, chalb, chal_unid, lep_pamale, 
            lep_male, lep_nongravid, lep_gravid, lep_pafemale,
            cal_mot, cal_gravid, unid_adult, unid_pa)
        } else {
          c(lep_cope, lep_pamale, 
            lep_male, lep_nongravid, lep_gravid, lep_pafemale,
            cal_mot, cal_gravid, unid_adult, unid_pa, cal_cope)
        }
        , na.rm = TRUE),
      all_cope = lep_cope + cal_cope,
      date = lubridate::make_date(year, month, day)
      ) %>% 
    # keep only the columns we want
    dplyr::select( 
      year, all_lep, all_lice, all_cope, lep_cope
    ) %>%
    dplyr::group_by(
      year
    ) %>% 
    # find yearly means for the two we want
    dplyr::summarize( 
      mean_lep = mean(all_lep, na.rm = TRUE),
      mean_all = mean(all_lice, na.rm = TRUE)
    ) %>% 
    # keep this out since it's getting predicted
    dplyr::filter(
      year != 2001
    ) %>% 
    # find the proportion of all the lice that are leps
    dplyr::mutate( 
      prop_lep = mean_lep / mean_all
    )
}








x = scfs_df %>% 
  # remove bad characters and make names standrdized
  standardize_names() %>% 
  dplyr::rowwise() %>% 
  select(year, unid_cope, lep_cope, cal_cope, lep_cope, chala, chalb, chal_unid, lep_pamale, 
         lep_male, lep_nongravid, lep_gravid, lep_pafemale,
         cal_mot, cal_gravid, unid_adult, unid_pa) %>% 
  group_by(year) %>% 
  summarize_all(mean, na.rm = TRUE)
y = scfs_df %>% 
  # remove bad characters and make names standrdized
  standardize_names() %>% 
  dplyr::rowwise() %>% 
  select(year, unid_cope, lep_cope, cal_cope, lep_cope, chala, chalb, chal_unid, lep_pamale, 
         lep_male, lep_nongravid, lep_gravid, lep_pafemale,
         cal_mot, cal_gravid, unid_adult, unid_pa) %>% 
  mutate_all(., ~coalesce(.,0)) %>% 
  mutate(year = as.factor(year)) %>% 
  group_by(year) %>%
  summarize_all(mean, na.rm = TRUE)

y1 = y %>% 
  group_by(year)
  summarize_all(mean, na.rm = TRUE)



x = scfs_df %>% 
  standardize_names() %>% 
  dplyr::rowwise() %>% 
  select(year, unid_cope, lep_cope, cal_cope, lep_cope, chala, chalb, chal_unid, lep_pamale, 
         lep_male, lep_nongravid, lep_gravid, lep_pafemale,
         cal_mot, cal_gravid, unid_adult, unid_pa) %>% 
  mutate_all(., ~coalesce(.,0)) %>% 
  mutate(year = as.factor(year)) %>% 
  rowwise() %>% 
  dplyr::mutate( # make columns that divide the lice into species 
    all_lep_mot = sum(
        c(lep_pamale, lep_male, lep_nongravid, 
          lep_gravid, lep_pafemale, unid_pa),
        na.rm = TRUE),
    all_lice_mot = sum(
        c(lep_pamale, 
          lep_male, lep_nongravid, lep_gravid, lep_pafemale,
          cal_mot, cal_gravid, unid_pa),
        na.rm = TRUE),
    all_cope = lep_cope + cal_cope
  ) %>% 
  as_tibble()
  
x1 = x %>% 
  # keep only the columns we want
  dplyr::select( 
    year, all_lep_mot, all_lice_mot, all_cope, lep_cope
  ) %>% 
  group_by(year) %>% 
  summarize_all(mean, na.rm =TRUE) %>% 
  filter(year != 2001) %>% 
  rowwise() %>% 
  mutate(
    prop_lep_mot = all_lep_mot/all_lice_mot,
    prop_lep_cope = lep_cope/all_cope
  )

xprop = x1 %>% 
  select(year, prop_lep_mot, prop_lep_cope) %>% 
  rename(mot = prop_lep_mot, cope = prop_lep_cope) %>% 
  pivot_longer(c(mot, cope)) %>% 
  rename(prop = value)
xall = x1 %>% 
  select(year, all_lice_mot, all_cope) %>% 
  rename(mot = all_lice_mot, cope = all_cope) %>% 
  pivot_longer(c(mot, cope)) %>% 
  rename(all = value)

xbind = cbind(xprop, xall$all) %>% 
  rename(all = `xall$all`)

# both proportions
ggplot(data = xbind,
       aes(x = all, y = prop, colour = name, label = year)) +
  geom_point() +
  geom_text()

# cope proportions vs mot proportions
ggplot(data = x1) + 
  geom_point(aes(x = prop_lep_cope, y = prop_lep_mot))


y1 = y %>% 
  rowwise() %>% 
  mutate(ratio = sum(chala + chalb + chal_unid)/sum(unid_cope, lep_cope, cal_cope))
hist(y1$ratio)
