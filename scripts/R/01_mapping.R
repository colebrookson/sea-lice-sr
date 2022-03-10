########## 
##########
# Look at locations of all data present to compare farm sampling and wild 
# sampling 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(raster)
library(maps)
library(ggrepel)


farm_data = read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations = read_csv(here("./data/raw/farm-locations.csv"))

# map of farm locations ========================================================

# get size of each farm 
farm_summarized = farm_data %>%
    rowwise() %>%
    dplyr::mutate(all_lice = lep.av + cal.av) %>%
    dplyr::group_by(farm) %>%
    dplyr::summarize(mean_size = mean(all_lice))

# add location data 
farm_loc_mean = inner_join(
    x = farm_summarized,
    y = farm_locations,
    by = "farm"
)

# get map data 
province = "British Columbia"
canada <- getData("GADM",country="CAN",level=1)

# subset to just BC
canada_prov = canada[canada$NAME_1 %in% province]



ggplot() +
    geom_polygon(data = canada_prov, 
        aes(x = long, y = lat, group = group), 
        colour = "black", 
        size = 0.01,
        fill = "grey65") + 
    coord_cartesian(xlim = c(-126.8,-126.1), ylim = c(50.5,50.9)) +
    theme_small_map() + 
    geom_point(data = farm_locations, aes(x = long, y = lat, label = farm),
                    size = 3, fill = "red", colour = "black", shape = 21) +
    geom_text_repel(data = farm_locations, aes(x = long, y = lat, label = farm))


