########## 
##########
# Pull in all data for cleaning 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# pull in file with all functions to clean data 
source(here("./src/02_data_cleaning_funs.R"))

pink_study = readr::read_csv(
    here("./data/louse-data/raw-scfs-data/pink_study_2001_data.csv")
)
lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"

scfs_data_raw = readr::read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_fishData.csv")))
scfs_data = standardize_names(scfs_data_raw)

# filter data ==================================================================

pink_sites = pink_study %>% 
    dplyr::filter(location %in% c("Glacier Falls", "Burdwood Islands", 
                            "Wicklow Point")) %>% # keep only important sites
    dplyr::filter(`Relation to Farm` == "near farm") %>% 
    # note that the Date column has a bunch of formats and I'm not totally 
    # sure if I can trust it so I'll use the day and month options instead
    dplyr::select(`Fish #`, day, mo, location, `length cm`, `Ch/ I-II-Lep`,
            `Ch/III-IV-Lep`, `unid subadult`, `CI-IV Caligus`, 
            `preadult 1 Lep-male`, `preadult 1 Lep-female`, 
            `preadult 1 Lep-female`, `preadult 2 Lep-male`, 
            `preadult 2 Lep-female`, `unid adult`, `adult male-Lep`,
            `virgin-Lep`, `adult female non-gravid-Lep`, `gravid female-Lep`,
            `adult male-Caligus`, `adult female-Caligus`,`gravid Caligus`)
            #%>% 
    #dplyr::rename(`Fish #`, day, mo, location, `length cm`, `Ch/ I-II-Lep`,
     #       `Ch/III-IV-Lep`, `unid subadult`, `CI-IV Caligus`, 
      #      `preadult 1 Lep-male`, `preadult 1 Lep-female`, 
       #     `preadult 1 Lep-female`, `preadult 2 Lep-male`, 
        #    `preadult 2 Lep-female`, `unid adult`, `adult male-Lep`,
         #   `virgin-Lep`, `adult female non-gravid-Lep`, `gravid female-Lep`,
          #  `adult male-Caligus`, `adult female-Caligus`,`gravid Caligus`)

# attempt to see if data match up  =============================================

match_vec = c()

# attempt to do jenky check one by one by a search 
temp_pink = pink_sites %>%
    dplyr::rowwise() %>% 
    dplyr::mutate(
        length_mm = `length cm` * 10
    ) %>% 
    dplyr::select(
        day, mo, location, length_mm
    )
temp_pink_glacier = pink_sites %>% 
    dplyr::filter(location == "Glacier Falls")
temp_pink_burd = pink_sites %>% 
    dplyr::filter(location == "Burdwood Islands") 
temp_pink_wick = pink_sites %>% 
    dplyr::filter(location == "Wicklow Point")

temp_scfs = scfs_data

temp_scfs = temp_scfs %>% 
    dplyr::filter(year == 2001) %>% 
    dplyr::filter(species == "pink")

# loop through the rows and see if a matching one exists
for(i in seq_len(nrow(temp_pink))) {

    pink_row = temp_pink[i,c("day", "mo", "length_mm")] # get just the row
    
}
