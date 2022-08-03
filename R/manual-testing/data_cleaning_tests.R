library(here)
library(tidyverse)
library(zoo)
source("./R/functions_farm_data_cleaning.R")

clean_data_marty(
 raw_file = here::here("./data/farm-data/raw/marty-2010-data/marty-data-copied.csv"),
 dfo_file = here::here("./data/farm-data/raw/farm-name-reference.csv"),
 output_path = here::here("./data/farm-data/clean/marty-data-clean.csv")
)



bati_df = get_data_bati(here::here(
 "./data/farm-data/raw/BATI_farm_louse_data_raw.csv"))
bati_df = standardize_names(bati_df)
dfo_names = get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv"))
bati_names = farm_names_bati(bati_df, dfo_names)
write_data_bati(bati_names, here::here("./data/farm-data/clean/bati-data-cleaned.csv"))
bati_df %>%
 standardize_names() %>%
 farm_names_bati(.,  get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv"))) %>%
 write_data_bati(., here::here("./data/farm-data/clean/bati-data-cleaned.csv"))

clean_data_bati(
  here::here("./data/farm-data/raw/BATI_farm_louse_data_raw.csv"),
  get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv")),
  here::here("./data/farm-data/clean/bati-data-cleaned.csv"))

dfo_df_not_trimmed = read_csv(here::here(
  paste0("./data/farm-data/raw/canadian-gov-open-data/",
         "fish-farm-sea-louse-counts-data.csv"))) %>% 
  rename(year = Year, month = Month, ref = `Facility Reference Number`,
         lep_av = `Average L. salmonis females per fish`,
         farm_name = `Site Common Name`) %>% 
  select(year, month, ref, lep_av, farm_name)

dfo_df = read_csv(here::here(
  paste0("./data/farm-data/raw/canadian-gov-open-data/",
         "fish-farm-sea-louse-counts-data.csv"))) %>% 
  rename(year = Year, month = Month, ref = `Facility Reference Number`,
         lep_av = `Average L. salmonis females per fish`) %>% 
  select(year, month, ref, lep_av) %>% 
  left_join(.,
            data.frame(
              month = unique(.$month),
              month_num = seq(1,12,1)
            )) %>% 
  select(-month) %>% 
  rename(month = month_num)


bati_df = read_csv(here::here("./data/farm-data/clean/bati-data-clean.csv"))


bati_df_ts = bati_df %>% 
  filter(month %in% c(3,4)) %>% 
  group_by(farm, year) %>% 
  summarize(mean_inventory = mean(inventory, na.rm = TRUE)) %>% 
  mutate(farm = as.factor(farm)) %>% 
  as_tibble()
bati_df_ts_comp = tidyr::complete(bati_df_ts, farm, year)
# bati_df_ts_comp$date = zoo::as.yearmon(
#   paste0(bati_df_ts_comp$year, 
#          bati_df_ts_comp$month), "%Y %m")





match_inventory_data = function(bati_df, dfo_df) {
  
  #' Check when we have no inventory if there is a lice measurement for that 
  #' time period 
  
  dfo_df1 = dfo_df %>% 
    dplyr::filter(month %in% c(3,4)) %>% 
    group_by(month, year, ref) %>% 
    summarize(lep_av = mean(lep_av, na.rm = TRUE)) %>% 
    ungroup()
  
  # make df with the lep mean measures for the months that there is at least 
  # one count for 
  value_holder = 
    complete(dfo_df1, year, month, ref) %>% 
    mutate(inventory = NA)
  
  for(row in seq_len(nrow(value_holder))) {
    bati_df %>% 
      filter(year == value_holder$year[row] &
               month == value_holder$month[row] & 
               ref == value_holder$ref[row])
  }
  
  
  
  
}

ggplot(data = bati_df_ts_comp) +
  geom_point(aes(x = year, y = mean_inventory, colour = farm)) +
  geom_line(aes(x = year, y = mean_inventory, colour = farm))
