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
library(PNWColors)

# include themes script
source(here("./src/01_plot_themes.R"))

farm_data = read_csv(here("./data/raw/BATI_farm_louse_data_RAW.csv"))
farm_locations = read_csv(here("./data/raw/farm-locations.csv"))

lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"
lice_site_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# pull together data for mapping ===============================================

# get size of each farm
farm_summarized = farm_data %>% # nolint
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
# add sampling status
sampled = c("Wicklow Point", "Burdwood", "Glacier Falls")
farm_loc_mean = farm_loc_mean %>%
    mutate(sampled =
        ifelse(farm_loc_mean$farm %in% sampled,
                "sampled", # if
                "unsampled")) # else
sampled_farms = farm_loc_mean %>%
    filter(sampled == "sampled")

# get map data
province = "British Columbia"
canada <- getData("GADM", country = "CAN", level = 1,
                    path = here("./data/geo-data"))
canada_prov = canada[canada$NAME_1 %in% province] # subset to just BC

# make maps ====================================================================

# set palette
size_pal = rev(pnw_palette("Moth", 4, type = "continuous"))

# create full plot
system_map = ggplot() +
    geom_polygon(data = canada_prov,
        aes(x = long, y = lat, group = group),
        colour = "black",
        size = 0.01,
        fill = "grey65") +
    coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
    geom_point(data = farm_loc_mean,
                aes(x = long, y = lat,
                    size = mean_size,
                    fill = mean_size),
                    shape = 21) +
    geom_text_repel(data = farm_loc_mean,
                    aes(x = long, y = lat, label = farm, colour = sampled)) +
    scale_fill_gradientn("Mean # of Lice per Fish",
                            colors = size_pal, guide = "legend",
                                limits = c(0, 5),
                                breaks = c(0, 1, 2, 3, 4, 5)) +
    guides(fill = guide_legend(), size = guide_legend(), color = "none") +
    scale_size_continuous("Mean # of Lice per Fish",
                limits = c(0, 5), breaks = c(0, 1, 2, 3, 4, 5)) +
    scale_colour_manual(values = c("red", "black")) +
    theme_small_map() +
    labs(x = "Longitude", y = "Latitude")

ggsave(filename = here("./figs/farm-map.png"),
        plot = system_map,
        width = 10,
        height = 8,
        dpi = 600)