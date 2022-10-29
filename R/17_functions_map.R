########## 
##########
# Plot output from all the options we have 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-10-22
##########
##########


#############################
# read_geo_data() function
#############################
read_geo_data = function(geo_file) {
  
  #' Reads in geo data to remove download troubles
  
  geo_data = readRDS(geo_file)
  
  return(geo_data)
}

#############################
# read_geo_data() function
#############################
read_farm_helper_data = function(farm_file) {
  
  #' Reads in geo data to remove download troubles
  
  clean_farms = readr::read_csv(farm_file)
  
  return(clean_farms)
}

#############################
# clean_geo_data() function
#############################
clean_geo_data = function(geo_data) {
  
  #' Just keep the part of the map we want to plot 
  
  province = "British Columbia"
  canada_prov = geo_data[geo_data$NAME_1 %in% province] # subset to just BC
  
  return(canada_prov)
}

#############################
# set_up_data() function
#############################
 set_up_data = function(loc_data, clean_farms) {
   
   #' Takes in helper df of clean farms and the clean all-farms-joined data
   #' and returns the data ready for the mapping
   
   farms_focal <- clean_farms %>% 
     dplyr::select(farm_name, hump_sarg_doc, ktf, ref) %>% 
     unique()
   
   farm_locs <- rbind(rbind(dplyr::left_join(
     farms_focal %>% 
       dplyr::filter(farm_name %notin% c("NA_7", "NA_15")), 
     loc_data %>% 
       dplyr::select(ref, Latitude, Longitude) %>% 
       dplyr::rename(lat = Latitude, long = Longitude),
     by = "ref"
   ),
   data.frame(
     farm_name = c("NA_7", "NA_15", "Glacier Falls 2"),
     hump_sarg_doc = c(0, 0, 0),
     ktf = c(1, 0, 1),
     ref = c(NA, NA, NA),
     lat = c(50.84947, 50.74335, 50.80490),
     long = c(-126.5012, -126.6151, -126.4273)
   )) %>% 
     dplyr::rowwise() %>% 
     dplyr::mutate(
       group = ifelse(
           ktf == 1, 
           "KTF farm",
           "Non-KTF farm"
       )
     ) %>% 
     dplyr::ungroup() %>% 
     dplyr::mutate(farm_num = seq(1, nrow(.))),
     data.frame(
       farm_name = c("glacier_samp", "burd_samp", "wick_samp"),
       hump_sarg_doc = c(0,0,0),
       ktf = c(0,0,0),
       ref = c(NA, NA, NA),
       lat = c(50.83785, 50.80690, 50.77659),
       long = c(-126.3292, -126.4958, -126.6915),
       group = c("Sampling Site", "Sampling Site", "Sampling Site"),
       farm_num = c(NA, NA, NA)
       )) %>% 
     dplyr::rowwise() %>% 
     dplyr::mutate(
       samp = as.factor(ifelse(group == "Sampling Site", 1, 0))
     )
   
   return(farm_locs)
   
 }
 
 #############################
 # set_up_data() function
 #############################
 make_map = function(canada_prov, farm_locs, output_file) {
   
   #' make the map with the geo-data and the farm data
   
   map =  ggplot() +
     geom_polygon(data = canada_prov,
                  aes(x = long, y = lat, group = group),
                  colour = "black",
                  size = 0.01,
                  fill = "grey65") +
     coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
     geom_point(data = farm_locs,
                aes(x = long, y = lat, fill = group, shape = samp),
                size = 4) +
     geom_text_repel(data = farm_locs,
                     aes(x = long, y = lat, 
                         label = farm_num
                     )) +
     scale_fill_manual("Farm Group", 
                       values = c("purple", "#078282FF", "orange", "grey70")) +
     scale_shape_manual("Farm Group",
                       values = c(21, 22)) +
     ggthemes::theme_base() +
     labs(x = "Longitude (°)", y = "Latitude (°)") +
     theme(
       legend.background = element_rect(fill="white",
                                        size=0.5, linetype="solid", 
                                        colour ="black"),
       legend.position = c(0.13, 0.12),
       legend.text = element_text(size = 12)
     ) +
     guides(
       fill = guide_legend(
         override.aes = list(size = 4, shape = c(21, 21, 22))
       ), 
       shape = "none"
     )
   
   ggsave(output_file, map, 
          width = 8, height = 6)
 }
 
 #############################
 # all_make_map_tasks() function
 #############################
 all_make_map_tasks = function(geo_file, farm_file, clean_farms, output_file) {
   
   #' Helper function to put it all together
   
   # set up the location stuff
   canada_prov = clean_geo_data(read_geo_data(geo_file))
   
   # clean and prepare data
   farm_locs = set_up_data(read_farm_helper_data(farm_file), clean_farms)
   
   # make the map
   make_map(canada_prov, farm_locs, output_file)
   
 }

# library(here)
# library(tidyverse)
# 
# clean_farms=read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
# loc_data <- read_csv(here("./data/farm-data/raw/farm-name-reference.csv"))
# farm_locs = set_up_data(loc_data, clean_farms)










