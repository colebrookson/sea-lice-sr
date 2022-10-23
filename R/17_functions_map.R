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
read_geo_data = function(file) {
  
  #' Reads in geo data to remove download troubles
  
  geo_data = readRDS(file)
  
  return(geo_data)
}

#############################
# read_geo_data() function
#############################
read_farm_helper_data = function(file) {
  
  #' Reads in geo data to remove download troubles
  
  clean_farms = readr::read_csv(file)
  
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
     dplyr::select(farm_name, hump_sarg_doc, ktc, ref) %>% 
     unique()
   
   farm_locs <- rbind(dplyr::left_join(
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
     ktc = c(1, 0, 1),
     ref = c(NA, NA, NA),
     lat = c(50.84947, 50.74335, 50.80490),
     long = c(-126.5012, -126.6151, -126.4273)
   )) %>% 
     dplyr::rowwise() %>% 
     dplyr::mutate(
       group = ifelse(
         farm_name %in% c("Wicklow Point", "Burdwood", "Glacier Falls"),
         "Sampling Site",
         ifelse(
           ktc == 1, 
           "KTF farm",
           "Non-KTF farm"
         )
       )
     ) %>% 
     dplyr::ungroup() %>% 
     dplyr::mutate(farm_num = seq(1, nrow(.)))
   
   return(farm_locs)
   
 }

library(here)
library(tidyverse)

clean_farms <- read_csv(here("./data/farm-data/clean/all-farms-joined-clean.csv"))
loc_data <- read_csv(here("./data/farm-data/raw/farm-name-reference.csv"))

farms_focal <- clean_farms %>% 
  dplyr::select(farm_name, hump_sarg_doc, ktc, ref) %>% 
  unique()

farm_locs <- rbind(dplyr::left_join(
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
    ktc = c(1, 0, 1),
    ref = c(NA, NA, NA),
    lat = c(50.84947, 50.74335, 50.80490),
    long = c(-126.5012, -126.6151, -126.4273)
  )) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(
    group = ifelse(
        farm_name %in% c("Wicklow Point", "Burdwood", "Glacier Falls"),
        "Sampling Site",
        ifelse(
          ktc == 1, 
          "KTF farm",
        "Non-KTF farm"
      )
    )
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(farm_num = seq(1, nrow(.)))


clean_geo_data = function()




ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               size = 0.01,
               fill = "grey65") +
  coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
  geom_point(data = farm_locs, 
             aes(x = Longitude, y = Latitude), shape = 22) + 
  geom_text_repel(data = all_farm_locations,
                  aes(x = Longitude, y = Latitude, 
                      label = name#, colour = sampled
                  )) 


ggplot() +
  geom_polygon(data = canada_prov,
               aes(x = long, y = lat, group = group),
               colour = "black",
               size = 0.01,
               fill = "grey65") +
  coord_cartesian(xlim = c(-127, -126.1), ylim = c(50.5, 50.9)) +
  geom_point(data = farm_locs,
             aes(x = long, y = lat, fill = group), shape = 21,
             size = 4) +
  geom_text_repel(data = farm_locs,
                  aes(x = long, y = lat, 
                      label = farm_num
                  )) +
  scale_fill_manual("Farm Group", 
                    values = c("purple", "#078282FF", "orange")) +
  ggthemes::theme_base() +
  labs(x = "Longitude", y = "Latitude") +
  theme(
    legend.background = element_rect(fill="white",
                                     size=0.5, linetype="solid", 
                                     colour ="black"),
    legend.position = c(0.13, 0.2)
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(size = 4)
    )
  )










