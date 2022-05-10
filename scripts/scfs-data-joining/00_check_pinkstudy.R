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
    ) %>% 
    dplyr::rename(month = mo)

temp_pink_glacier = temp_pink %>% 
    dplyr::filter(location == "Glacier Falls")
c = temp_pink %>% 
    dplyr::filter(location == "Burdwood Islands") 
temp_pink_wick = temp_pink %>% 
    dplyr::filter(location == "Wicklow Point")

temp_scfs = scfs_data

temp_scfs = temp_scfs %>% 
    dplyr::filter(year == 2001) %>% 
    dplyr::filter(species == "pink") %>% 
    dplyr::select(day, month, length, location)

temp_scfs_glacier = temp_scfs %>% 
    dplyr::filter(location == "Glacier")
temp_scfs_burd = temp_scfs %>% 
    dplyr::filter(location == "Burdwood")
temp_scfs_wick = temp_scfs %>% 
    dplyr::filter(location == "Wicklow")

# write out sorted pink study data
sorted_temp_pink_glacier = arrange(temp_pink_glacier,
    desc(day), desc(month), desc(length_mm))
write_csv(sorted_temp_pink_glacier,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_pink_glacier.csv")
)
sorted_temp_pink_burd= arrange(temp_pink_burd,
    desc(day), desc(month), desc(length_mm))
write_csv(sorted_temp_pink_burd,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_pink_burd.csv")
)
sorted_temp_pink_wick= arrange(temp_pink_wick,
    desc(day), desc(month), desc(length_mm))
write_csv(sorted_temp_pink_wick,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_pink_wick.csv")
)
# write out sorted other data
sorted_temp_scfs_glacier = arrange(temp_scfs_glacier,
    desc(day), desc(month), desc(length))
write_csv(sorted_temp_scfs_glacier,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_scfs_glacier.csv")
)
sorted_temp_scfs_burd = arrange(temp_scfs_burd,
    desc(day), desc(month), desc(length))
write_csv(sorted_temp_scfs_burd,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_scfs_burd.csv")
)
sorted_temp_scfs_wick= arrange(temp_scfs_wick,
    desc(day), desc(month), desc(length))
write_csv(sorted_temp_scfs_wick,
    here("./data/louse-data/matching-scfs-pink-study-data/
    sorted_temp_scfs_wick.csv")
)


