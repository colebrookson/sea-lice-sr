library(here)
library(tidyverse)

source("./R/01_functions_farm_data_cleaning.R")


# clean_data_marty(
#  raw_file = here::here("./data/farm-data/raw/marty-2010-data/marty-data-copied.csv"),
#  dfo_file = here::here("./data/farm-data/raw/farm-name-reference.csv"),
#  output_path = here::here("./data/farm-data/clean/marty-data-clean.csv")
# )

# read in a bunch of data ======================================================

marty_df = read_csv(here("./data/farm-data/clean/marty-data-clean.csv"))
dfo_df = read_csv(here::here(
  paste0("./data/farm-data/raw/gov-open-data/",
         "fish-farm-sea-louse-counts-data.csv")))
bati_df = get_data_bati(here::here(
 "./data/farm-data/raw/BATI_farm_louse_data_raw_to_2022.csv"))
dfo_names = get_data_dfo_ref(here::here("./data/farm-data/raw/farm-name-reference.csv"))


# clean some names from the dfs ================================================
bati_df = standardize_names(bati_df)
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
         lep_av = `Average L. salmonis females per fish`,
         farm_name = `Site Common Name`) %>% 
  select(year, month, ref, lep_av, farm_name) %>% 
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
  
  # make df with the lep mean measures for the months that there is at least 
  # one count for 
  dfo_df %>% 
    # rename the columns we care about in the dfo data 
    dplyr::rename(year = Year, month = Month, ref = `Facility Reference Number`,
           lep_av = `Average L. salmonis females per fish`,
           farm_name = `Site Common Name`) %>% 
    # keep only relevant columns
    dplyr::select(year, month, ref, lep_av, farm_name) %>% 
    # perform quick join to get months in number format
    dplyr::left_join(.,
              data.frame(
                month = unique(.$month),
                month_num = seq(1,12,1)
              )) %>% 
    dplyr::select(-month) %>% 
    dplyr::rename(month = month_num) %>% 
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
    dplyr::mutate(
      lep_tot = inventory * lep_av,
    )
}

ggplot(data = bati_df_ts_comp) +
  geom_point(aes(x = year, y = mean_inventory, colour = farm)) +
  geom_line(aes(x = year, y = mean_inventory, colour = farm))
