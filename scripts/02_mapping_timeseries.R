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
library(zoo)
library(patchwork)

# include themes script
source(here("./src/01_plot_themes.R"))
source(here("./src/02_data_cleaning_funs.R"))

# set file locations
lice_file_location = "./data/louse-data/Sea-lice-database-master/Data/"
farm_file_location = "./data/farm-data/raw/canadian-gov-open-data/"

# read in data 
raw_marty_data = readxl::read_excel(
    path = here("./data/farm-data/raw/marty-2010-data/sd01.xlsx"),
    sheet = 2
)
dfo_open_data = read_csv(here(paste0(farm_file_location, 
                                "fish-farm-sea-louse-counts-data.csv")))
farm_locations_df = read_csv(here::here("./data/farm-data/raw/farm-locations.csv"))
farm_regress = read_csv(
  here::here("./data/farm-data/clean/marty-bati-data-joined-stocked-only.csv"))
all_farm_locations = read_csv(here::here("./data/farm-data/raw/farm-name-reference.csv"))

# pull together data for mapping ===============================================

# put together relevant data
farm_loc = bind_map_data(raw_marty_data, farm_locations_df, dfo_open_data,
                            c("Wicklow Point", "Burdwood", "Glacier Falls (1)"))

# write out combos of farm names and farm numbers 
farm_map_nums = farm_loc %>% 
    # keep relevant columns
    dplyr::select(farm_name, farm_num) %>% 
    # ensure both galcier falls are categorized together 
    dplyr::rowwise() %>% 
    dplyr::mutate(
        farm_name = ifelse(farm_name %in% 
                c("Glacier Falls (1)", "Glacier Falls (2)"),
            "Glacier Falls", farm_name),
        farm_num = ifelse(farm_name == "Glacier Falls", 6, farm_num)
    ) %>% 
    unique()
write_csv(farm_map_nums, 
        here("./data/clean-farm/farm-numbers-names-according-to-map.csv"))

# make factors
farm_loc$ktc = as.factor(farm_loc$ktc)
farm_loc$hump_sarg_doc = as.factor(farm_loc$hump_sarg_doc)
farm_loc$sampled = as.factor(farm_loc$sampled)

# get map data
province = "British Columbia"
canada <- raster::getData("GADM", country = "CAN", level = 1,
                    path = here("./data/geo-data"))
canada_prov = canada[canada$NAME_1 %in% province] # subset to just BC

# make maps ====================================================================

# set palette
size_pal = rev(pnw_palette("Moth", 4, type = "continuous"))
ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               size = 0.01,
               fill = "grey65") +
  coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
  geom_point(data = all_farm_locations, 
             aes(x = Longitude, y = Latitude), shape = 22) + 
  geom_text_repel(data = all_farm_locations,
                  aes(x = Longitude, y = Latitude, 
                      label = name#, colour = sampled
                  )) 

# create full plot
#system_map = 
ggplot() +
    geom_polygon(data = canada_prov,
        aes(x = long, y = lat, group = group),
        colour = "black",
        size = 0.01,
        fill = "grey65") +
    coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
    geom_point(data = farm_loc,
                aes(x = long, y = lat,
                    fill = ktc,
                    shape = sampled,
                    size = sampled)) +
  geom_point(data = all_farm_locations, aes(x = Longitude, y = Latitude), shape = 22) + 
    geom_text_repel(data = farm_loc,
                    aes(x = long, y = lat, 
                        label = farm_num#, colour = sampled
                        )) +
    scale_fill_manual("Knight Tribune Corridor Classification", 
                        values = c("#963CBDFF", "#FF6F61FF")) +
    scale_shape_manual("Sampling Site Near Farm", 
                        values = c(23, 21),
                        labels = c("Sampling Site", "No Sampling Site")) +
    scale_size_manual(values = c(5.5, 3)) +
    guides(fill = guide_legend(
        override.aes = list(size = 4, shape = 21)
        ),
    shape = guide_legend(
        override.aes = list(size = 4)
        ),
    size = "none") +
    #scale_colour_manual(values = c("red", "black")) +
    theme_small_map() +
    labs(x = "Longitude", y = "Latitude")

ggsave(filename = here("./figs/numbered-farm-map.png"),
        plot = system_map,
        width = 10,
        height = 8,
        dpi = 600)

# farm timeseries ==============================================================

farm_regress$year_fac = as.factor(farm_regress$year)
farm_regress$month = as.factor(farm_regress$month)

# make dataset with all farms
farm_timeseries = farm_regress %>% 
  dplyr::filter(farm_name %notin% c("Arrow Pass", "NA_15", "NA_7", "NA_14")) %>% 
  #dplyr::filter(year < 2017) %>% 
  dplyr::group_by(year_fac, month) %>% 
  summarize(sum_invent = sum(inventory)/1000000,
            sum_lep = sum(lep_tot)/1000000)
farm_timeseries$date = zoo::as.yearmon(paste(farm_timeseries$year_fac, 
                                             farm_timeseries$month), "%Y %m")
farm_timeseries$class = "All"

# get dataset with only ktc farms
farm_timeseries_ktc = farm_regress %>% 
  dplyr::filter(ktc == "Knight Tribune Corridor") %>% 
  #dplyr::filter(year < 2017) %>% 
  dplyr::group_by(year_fac, month) %>% 
  summarize(sum_invent = sum(inventory)/1000000,
            sum_lep = sum(lep_tot)/1000000)
farm_timeseries_ktc$date = zoo::as.yearmon(paste(farm_timeseries_ktc$year_fac, 
                                                 farm_timeseries_ktc$month), 
                                           "%Y %m")
farm_timeseries_ktc$class = "KTC"

# get dataset with only NOT ktc farms
farm_timeseries_br = farm_regress %>% 
  dplyr::filter(ktc != "Knight Tribune Corridor") %>% 
  #dplyr::filter(year < 2017) %>% 
  dplyr::group_by(year_fac, month) %>% 
  summarize(sum_invent = sum(inventory)/1000000,
            sum_lep = sum(lep_tot)/1000000)
farm_timeseries_br$date = zoo::as.yearmon(paste(farm_timeseries_br$year_fac, 
                                                farm_timeseries_br$month), 
                                          "%Y %m")
farm_timeseries_br$class = "Non-KTC"

if(all(names(farm_timeseries) == names(farm_timeseries_ktc)) & 
   all(names(farm_timeseries) == names(farm_timeseries_br))) {
  all_farm_ts = rbind(farm_timeseries_ktc, farm_timeseries_br, farm_timeseries)
  all_farm_ts$class = as.factor(all_farm_ts$class)
} else {
  stop("ERROR - columns not matching")
}

# plot with three farm classifications
fish_ts_plot_v1 = ggplot(data = all_farm_ts) +
  geom_line(aes(x = date, y = sum_invent, linetype = class, alpha = class)) +
  geom_point(aes(x = date, y = sum_invent, fill = class, alpha = class,
                 shape = class), size = 3)+
  theme_area_grouping() +
  scale_alpha_manual(values = c(1, 0.3, 0.3)) +
  scale_shape_manual(values = c(21, 22, 23)) + 
  labs(x = "Date", y = "Farmed Farmed Salmon (millions)") +
  scale_fill_manual("Farm", values = c("purple", "orange1", "lightblue1")) +
  guides(
    alpha = "none",
    linetype = "none",
    fill = guide_legend(override.aes = list(shape = c(21, 22, 23))),
    shape = "none"
  )
ggsave(here::here("./figs/fish-on-farms-timeseries.png"), fish_ts_plot_v1,
       width = 10, height = 5)

# make version with no x axis text
fish_ts_plot_v2 = ggplot(data = all_farm_ts) +
  geom_line(aes(x = date, y = sum_invent, linetype = class, alpha = class)) +
  geom_point(aes(x = date, y = sum_invent, fill = class, alpha = class,
                 shape = class), size = 3)+
  theme_area_grouping() +
  scale_alpha_manual(values = c(1, 0.3, 0.3)) +
  scale_shape_manual(values = c(21, 22, 23)) + 
  labs(x = "", y = "Farmed Salmon (millions)") +
  scale_fill_manual("Farm", values = c("purple", "orange1", "lightblue1")) +
  theme(
    axis.text.x = element_blank(),
    axis.title.y = element_text(size = 18)
  ) +
  guides(
    alpha = "none",
    linetype = "none",
    fill = guide_legend(override.aes = list(shape = c(21, 22, 23))),
    shape = "none"
  )

lice_ts_plot = ggplot(data = all_farm_ts) +
  geom_line(aes(x = date, y = sum_lep, linetype = class, alpha = class)) +
  geom_point(aes(x = date, y = sum_lep, fill = class, alpha = class,
                 shape = class), size = 3) +
  theme_area_grouping() +
  scale_alpha_manual(values = c(1, 0.3, 0.3)) +
  scale_shape_manual(values = c(21, 22, 23)) +
  labs(x = "Date", y = "Lice on Farmed Salmon (millions)") +
  scale_fill_manual("Farm", values = c("purple", "orange1", "lightblue1")) +
  theme(
    axis.title.y = element_text(size = 18)
  ) +
  guides(
    alpha = "none",
    linetype = "none",
    fill = guide_legend(override.aes = list(shape = c(21, 22, 23))),
    shape = "none"
  )
ggsave(here::here("./figs/lice-on-farms-timeseries.png"), lice_ts_plot,
       width = 10, height = 5)

# add both plots together
ts_both = fish_ts_plot_v2 / lice_ts_plot
ggsave(here::here("./figs/fish-and-lice-on-farms-timeseries.png"), ts_both,
       width = 10, height = 10)

