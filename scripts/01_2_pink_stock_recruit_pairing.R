##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(stringr)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

nuseds_raw = read_csv(here::here("./data/NuSEDS/NuSEDS_20220309.csv"))
pink_exp = 
  read_csv(here::here("./data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = 
  read_csv(here::here("./data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = 
  read_csv(here::here(
    "./data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))


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

# empty escapment vector to put values in 
esc_vec = rep(NA, length(years))
yr_vec = numeric(length(years))
riv_vec = character(length(years))

# loop through all the year-river combos and get escapement values 
iter = 1
for(year in 1954:2017) {
  for(river in unique(nuseds$WATERBODY)) {
    # get the subset of the data we're interested in 
    temp = nuseds[which(nuseds$ANALYSIS_YR == year & 
                          nuseds$WATERBODY == river), ]
    # find number of rows we need to deal with 
    n_obs = nrow(temp)
    if(n_obs == 0) { # do zero case here
      # essentially just keep the current value of NA
      esc = -9999999      
    } else if(n_obs == 1) { # deal with cases of only one or less observations
      esc = 
        dplyr::case_when(
          n_obs == 1 ~
            # sub-case
            dplyr::case_when(
              is.na(temp$MAX_ESTIMATE) ~
                # sub-case when is.na is true
                dplyr::case_when(
                  # proper NA
                  temp$ADULT_PRESENCE %in% c("NOT INSPECTED",
                                             "UNKOWN") ~ temp$MAX_ESTIMATE,
                  # should be zero
                  temp$ADULT_PRESENCE == "NONE OBSERVED" ~ 0),
              # sub case if is.na isn't true
              !is.na(temp$MAX_ESTIMATE) ~ temp$MAX_ESTIMATE)
        )
    } else { # now deal with more than one observation 
      vals = temp$MAX_ESTIMATE
      for(obs in 1:nrow(temp)) {
        if(is.na(vals[obs])) {
          vals[obs] = 
            dplyr::case_when(
              temp$ADULT_PRESENCE[obs] %in% c("NOT INSPECTED", "UNKOWN") ~ 
                vals[obs],
              temp$ADULT_PRESENCE[obs] == "NONE OBSERVED" ~ 0
            )
        } else if(!is.na(vals[obs])) {
          vals[obs] = vals[obs]
        }
      }
      esc = sum(vals, na.rm = TRUE)
    }
    # now put each 
    esc_vec[iter] = esc
    yr_vec[iter] = year
    riv_vec[iter] = river
    # iterate
    iter = iter + 1
  }
}

# et rid of all -9999999 values
esc_vec[which(esc_vec == -9999999)] = NA

# now turn vectors into a dataframe
esc_df = data.frame(
  esc = esc_vec,
  river = riv_vec,
  year = yr_vec
)

# cut down nuseds data to just area and river to join
nuseds_areariver = nuseds %>% 
  dplyr::select(AREA, WATERBODY) %>% 
  dplyr::rename(river = WATERBODY)
esc_df = dplyr::left_join(
  x = esc_df,
  y = nuseds_areariver,
  by = "river"
)

# set up catch data ============================================================

# get a rate not percentage from English data
pink_exp$exp_rate = as.numeric(substr(pink_exp$exp_rate,
                           1,
                           nchar(pink_exp$exp_rate)-4))/100

# now add in area 12 data to the pink_exp df
area12_cus = c("Southern Fjords (even)", "Southern Fjords (odd)",
               "Homathko-Klinaklini (odd)", "Nahwitti", 
               "East Vancouver Island (odd)")
pink_area12 = pink_recon_rate %>% 
  dplyr::filter(conservation_unit %in% area12_cus)

# to apply exploitation as accurately as possible, put in all rivers that
# apply to each CU
rivers_helper_df = rbind(
  extract_rivers(pink_helper, "south_fjords_even"),
  extract_rivers(pink_helper, "south_fjords_odd"),
  extract_rivers(pink_helper, "east_vi"),
  extract_rivers(pink_helper, "hk"),
  extract_rivers(pink_helper, "nahwitti")
)
# make all upper case so they match
rivers_helper_df$rivers = stringr::str_to_upper(rivers_helper_df$rivers)

south_fjords_even_one = c("Eva Creek, Driftwood Creek, Pack Lake Creek, Rainbow Creek, Seymour River, Waump Creek, Blind Creek, Boughey Creek, Fulmore River, Robbers Knob Creek, Ahnuhati River, Ahta River, Ahta Valley Creek, Call Creek, Gilford Creek, Glendale Creek, Hoeya Sound Creek, Kakweiken River, Kamano Bay Creek, Klinaklini River, Kwalate Creek, Lull Creek, Maple Creek, Matsui Creek, McAlister Creek, Port Harvey Lagoon Creeks, Potts Lagoon Creek, Protection Point Creek, Sallie Creek, Shoal Harbour Creek, Sim River, Viner Sound Creek, Adam River, Charles Creek, Eve River, Hyde Creek, Kokish River, Mills Creek, Naka Creek, Nimpkish River, Stranby River, Thiemer Creek, Tsitika River, Tuna River, Bughouse Creek, Carriden Creek, Charles Creek, Cohoe Creek, Embley Creek, Hauskin Creek, Health Lagoon Creek, Jennis Bay Creek, Kingcome River, Mackenzie River, Nimmo Creek, Scott Cove Creek, Simoom Sound Creek, Wakeman River, Waldon Creek, Cluxewe River, Keogh River, Nahwitti River, Quatse River, Shushartie River, Songhees Creek, Stranby River, Tsulquate River")
south_fjords_even = stringr::str_split(south_fjords_even_one,
                                       ", ")
south_fjords_odd = c("Eva Creek, Driftwood Creek, Pack Lake Creek, Rainbow Creek, Seymour River, Waump Creek, Blind Creek, Boughey Creek, Fulmore River, Robbers Knob Creek,Ahnuhati River, Ahta River, Ahta Valley Creek, Call Creek, Gilford Creek, Glendale Creek, Hoeya Sound Creek, Kakweiken River, Kamano Bay Creek, Klinaklini River, Kwalate Creek, Lull Creek, Maple Creek, Matsui Creek, McAlister Creek, Port Harvey Lagoon Creeks, Potts Lagoon Creek, Protection Point Creek, Sallie Creek, Shoal Harbour Creek, Sim River, Viner Sound Creek")
east_vi = c("Adam River, Charles Creek, Eve River, Hyde Creek, Kokish River, Mills Creek, Naka Creek, Nimpkish River, Stranby River, Thiemer Creek, Tsitika River, Tuna River")
hk = c("Bughouse Creek, Carriden Creek, Charles Creek, Cohoe Creek, Embley Creek, Hauskin Creek, Health Lagoon Creek, Jennis Bay Creek, Kingcome River, Mackenzie River, Nimmo Creek, Scott Cove Creek,Simoom Sound Creek, Wakeman River, Waldon Creek")
nahwitti = c("Cluxewe River, Keogh River, Nahwitti River, Quatse River, Shushartie River, Songhees Creek, Stranby River, Tsulquate River")

## LEFT OFF FIGURING OUT THE BEST WAY TO SPLIT THESE STRINGS SO I CAN MATCH
# THEM TO THE DATABASE AND FIGURE OUT WHAT THE SPECIFIC EXPLOITATION RATES ARE

# get catch rate for the pink data from Pieter
pink_recon_rate = pink_recon %>% 
  rowwise() %>% 
  mutate(exp_rate = apportioned_catch/total_stock) %>% 
  select(conservation_unit, year, exp_rate, total_stock, apportioned_catch)



## NOTE - need to figure out how these relate to the CU's - seem to remember 
# this being on the PSF website somewhere in the PSE? 
unique(psf_pink$location)






## get exploitation rates from two sources of catch data 
## the pink reconstructions and the PSF data 

## Set up overall S-R data base with all rivers 

## Remove ambigious farm exposure/enhancement/etc

## make sure odd and even year populations are specified

## Only keep pop'ns with a minimum of 20 spawner-recruit pairs

## Include lice co-variate

## make final data base 


