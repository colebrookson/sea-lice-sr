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

farm_data = read_csv(here(
    "./data/raw/canadian-gov-open-data/fish-farm-sea-louse-counts-data.csv"
))

lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"
lice_site_data = read_csv(here(paste0(lice_file_location,
                                "BroughtonSeaLice_siteData.csv")))

# pull together data for mapping ===============================================

# only keep columns of interest 
farm_data_sum = farm_data %>% 
    dplyr::filter(
        `Finfish Aquaculture Reporting Zone` == "Broughton Archipelago") %>%
    dplyr::select(`Site Common Name`, Longitude, Latitude, 
        `Average L. salmonis motiles per fish`) %>%
    dplyr::rename(farm = `Site Common Name`, 
            lat = Latitude,
            long = Longitude,
            avg_leps = `Average L. salmonis motiles per fish`
            ) %>% 
    dplyr::mutate(farm = as.factor(farm)) %>% 
    dplyr::group_by(farm, lat, long) %>% 
    dplyr::summarize(
        mean_leps = mean(avg_leps, na.rm = TRUE)
    ) %>%
    dplyr::filter(lat > 0) %>% 
    dplyr::ungroup() # put back to `chr` for the ifelse

# add sampling status
sampled = c("Wicklow Point", "Burdwood", "Glacier Falls")
farm_loc = farm_data_sum %>%
    mutate(sampled =
        ifelse(farm_data_sum$farm %in% sampled,
                "sampled", # if
                "unsampled")) # else
sampled_farms = farm_loc %>%
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
    geom_point(data = farm_loc,
                aes(x = long, y = lat,
                    size = mean_leps,
                    fill = mean_leps),
                    shape = 21) +
    geom_text_repel(data = farm_loc,
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