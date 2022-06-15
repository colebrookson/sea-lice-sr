##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

nuseds_raw = read_csv(here::here("./data/NuSEDS/NuSEDS_20220309.csv"))

# trim nuseds data to useful size ==============================================
names(nuseds_raw)

names_keep = c("AREA", "WATERBODY", "POPULATION", "RUN_TYPE", "WATERSHED_CDE",
               "SPECIES", "ANALYSIS_YR", "MAX_ESTIMATE", "ADULT_PRESENCE")
nuseds = nuseds_raw %>% 
  select(all_of(names_keep)) %>% 
  filter(SPECIES == "Pink") %>% 
  filter(AREA %in% c("7", "8", "9", "10", "12"))

## get escapement by either taking the value from the year and river
## or summing the values (ensure to remove NA)
## also get the area from the river 

# pull escapement data from database ===========================================

# set up a vector for years to be the column in the dataframe 
if(length(unique(nuseds$WATERBODY)) != 279) {
  stop("ERROR: Number of Rivers Is Not Right! Should be 279")
}
years = rep(1954:2017, length(unique(nuseds$WATERBODY)))

# same with the rivers
rivers = rep(unique(nuseds$WATERBODY), each = length(1954:2017))

if(length(years) != length(rivers)) {
  stop("ERROR: length of rivers vector and years vector are not equal")
}

# set empty vectors for the other variables we will want 
esc_df = data.frame(
  years = as.factor(years), 
  rivers = as.factor(rivers), 
  escape = numeric(length(years)),
  area = numeric(length(years))
)

# loop through all the year-river combos and get escapement values 
for(year in 1954:2017) {
  for(river in unique(esc_df$rivers)){
    
    ## CASE 1 --- NO VALUE ##
    
    # if no escapement value for that year/river combo, fill with NA
    if( # check for combos with no observations
      nrow(nuseds[which(nuseds$ANALYSIS_YR == year & 
                        nuseds$WATERBODY == river), ]) < 1
    ) { # if no observations, fill with NA
      esc_df[which(esc_df$years == year &
                     esc_df$rivers == river), "escape"] = NA
    } 
    
    ## CASE 2 --- ONLY ONE VALUE ##
    
    if( # if only one row
      nrow(nuseds[which(nuseds$ANALYSIS_YR == year & 
                          nuseds$WATERBODY == river), ]) == 1
    ) { # use that observation
      
      ## CASE 2.1 --- NA IS THE VALUE ##
      
      if( # if the value is NA, then do one of a few things
        is.na(nuseds[which(nuseds$ANALYSIS_YR == year & 
                     nuseds$WATERBODY == river), "MAX_ESTIMATE"])
        ) {
          # if the status of "ADULT_PRESENCE" is not inspected or unknown
          # then just use the value (NA)
          if( 
            nuseds[which(nuseds$ANALYSIS_YR == year & 
                         nuseds$WATERBODY == river), "ADULT_PRESENCE"] %in%
            c("NOT INSPECTED", "UNKOWN")
          ) {
            esc_df[which(esc_df$years == year &
                           esc_df$rivers == river), "escape"] = 
              nuseds[which(nuseds$ANALYSIS_YR == year & 
                             nuseds$WATERBODY == river), "MAX_ESTIMATE"]
          }
          # if the status of "ADULT_PRESENCE" is none observed, then instead sub
          # in a zero 
          if(
            nuseds[which(nuseds$ANALYSIS_YR == year & 
                         nuseds$WATERBODY == river), "ADULT_PRESENCE"] ==
            "NONE OBSERVED"
          ) {
            esc_df[which(esc_df$years == year &
                           esc_df$rivers == river), "escape"] = 0
          }
        
        ## CASE 2.2 --- VALUE IS REAL ##
      }
      
      
      esc_df[which(esc_df$years == year &
                     esc_df$rivers == river), "escape"] = 
        nuseds[which(nuseds$ANALYSIS_YR == year & 
                       nuseds$WATERBODY == river), "MAX_ESTIMATE"]
    } if( # if more than one row, use both
      nrow(nuseds[which(nuseds$ANALYSIS_YR == year & 
                        nuseds$WATERBODY == river), ]) > 1
      
    ) {
      esc_df[which(esc_df$years == year &
                     esc_df$rivers == river), "escape"] = 
        sum(
          nuseds[which(nuseds$ANALYSIS_YR == year & 
                         nuseds$WATERBODY == river), ],
          na.rm = TRUE
        )
    } 
  }
}

## TEST
year = unique(esc_df$years)[1]
river = unique(esc_df$rivers)[1]

year = 2001
river = "SHUSHARTIE RIVER"
for(year in unique(esc_df$years)) {
  for(river in unique(esc_df$rivers)){
    
    if(
      nrow(nuseds[which(nuseds$ANALYSIS_YR == year &
                        nuseds$WATERBODY == river),]) > 1
    ) {
      print(year); print(river)
   }
    
  }
}

## get exploitation rates from two sources of catch data 
## the pink reconstructions and the PSF data 

## Set up overall S-R data base with all rivers 

## Remove ambigious farm exposure/enhancement/etc

## make sure odd and even year populations are specified

## Only keep pop'ns with a minimum of 20 spawner-recruit pairs

## Include lice co-variate

## make final data base 


