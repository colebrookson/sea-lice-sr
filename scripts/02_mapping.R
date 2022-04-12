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
source(here("./src/02_data_cleaning_funs.R"))

# set file locations
lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"
farm_file_location = "./data/raw-farm/canadian-gov-open-data/"

# read in data 
raw_marty_data = readxl::read_excel(
    path = here("./data/raw-farm/marty-2010-data/sd01.xlsx"),
    sheet = 2
)
dfo_open_data = read_csv(here(paste0(farm_file_location, 
                                "fish-farm-sea-louse-counts-data.csv")))
farm_locations_df = read_csv(here(here("./data/raw-farm/farm-locations.csv")))

# pull together data for mapping ===============================================
farm_loc = bind_map_data(raw_marty_data, farm_locations_df, dfo_open_data,
                            c("Wicklow Point", "Burdwood", "Glacier Falls (1)"))

# make factors
farm_loc$ktc = as.factor(farm_loc$ktc)
farm_loc$hump_sarg_doc = as.factor(farm_loc$hump_sarg_doc)
farm_loc$sampled = as.factor(farm_loc$sampled)

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
                    fill = ktc,
                    shape = sampled),
                    size = 3.5) +
    geom_text_repel(data = farm_loc,
                    aes(x = long, y = lat, 
                        label = farm_num#, colour = sampled
                        )) +
    scale_fill_manual("Knight Tribune Corridor Classification", 
                        values = c("#963CBDFF", "#FF6F61FF")) +
    scale_shape_manual("Sampling Site Near Farm", values = c(21, 24)) +
    guides(fill = guide_legend(
        override.aes = list(size = 4, shape = 21)
        ),
    shape = guide_legend(
        override.aes = list(size = 4)
        )) +
    #scale_colour_manual(values = c("red", "black")) +
    theme_small_map() +
    labs(x = "Longitude", y = "Latitude")

ggsave(filename = here("./figs/numbered-farm-map.png"),
        plot = system_map,
        width = 10,
        height = 8,
        dpi = 600)