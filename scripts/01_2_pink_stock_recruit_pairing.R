##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(patchwork)

# pull in file with all functions to clean data 
source(here::here("./src/02_data_cleaning_funs.R"))
source(here::here("./src/01_plot_themes.R"))

nuseds_raw = readr::read_csv(here::here("./data/NuSEDS/NuSEDS_20220309.csv"))
pink_exp = 
  readr::read_csv(here::here(
    "./data/dfo-data/raw/pink/english-report-translated.csv"))
pink_recon = 
  readr::read_csv(here::here("./data/dfo-data/clean/pink-reconstructions.csv"))
pink_helper = 
  readr::read_csv(here::here(
    "./data/dfo-data/raw/pink/helper-data-river-cu-match.csv"))
lice_pred = readr::read_csv(here::here(
  "./data/regression-data/predicted-lice-abundance.csv"))

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
  cbind(
  esc = esc_vec,
  river = riv_vec,
  year = yr_vec
  )
)

# cut down nuseds data to just area and river to join
nuseds_areariver = nuseds %>% 
  dplyr::select(AREA, WATERBODY) %>% 
  dplyr::rename(river = WATERBODY) %>% 
  unique()
esc_df = dplyr::left_join(
  x = esc_df,
  y = nuseds_areariver,
  by = "river"
  ) %>% 
  dplyr::rename(area = AREA)

# set up catch data ============================================================

# get a rate not percentage from English data
pink_exp$exp_rate = as.numeric(substr(pink_exp$exp_rate,
                           1,
                           nchar(pink_exp$exp_rate)-4))/100

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

# get catch rate for the pink data from Pieter
pink_recon_rate = pink_recon %>% 
  rowwise() %>% 
  mutate(exp_rate = apportioned_catch/total_stock) %>% 
  select(conservation_unit, year, exp_rate, total_stock, apportioned_catch)

# now add in area 12 data to the pink_exp df
area12_cus = c("Southern Fjords (even)", "Southern Fjords (odd)",
               "Homathko-Klinaklini (odd)", "Nahwitti", 
               "East Vancouver Island (odd)")
pink_area12 = pink_recon_rate %>% 
  dplyr::filter(conservation_unit %in% area12_cus)

# add exploitation rates to data ===============================================

# do it in an ugly loop so it's readable

esc_df_short = esc_df %>% 
  filter(year <= 2016)
esc_df_short$exp = NA
for(row in seq_len(nrow(esc_df_short))){
  
  # find the river, area, and year first
  cur_area = esc_df_short[row, "area"]
  cur_year = esc_df_short[row, "year"]
  cur_river = esc_df_short[row, "river"]
  
  # if the area is 7-10 it's easy just take the one value
  if(cur_area %in% c(7:10)) {
    esc_df_short[row, "exp"] = 
      pink_exp[which(pink_exp$year == cur_year & 
                       pink_exp$area == cur_area), "exp_rate"]
  } else if(cur_area == 12) { # if it's area 12 then it's a bit harder
    
    # first check to see if the specific river is in the helper df
    if(cur_river %in% rivers_helper_df$rivers) {
      region = rivers_helper_df[which(rivers_helper_df$rivers == cur_river),
                                "cu"]
      
      # if the river is in two regions - deal with that 
      if(length(region > 1)) {
        if((as.numeric(cur_year) %% 2) != 0) {# if odd year take out even region
          region = region[which(region != "south_fjords_even")]
        }
      }
      
      # if the river is in the helper df, find the corresponding 
      # area and put that value in 
      if(region == "south_fjords_even") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Southern Fjords (even)"), 
                      "exp_rate"]
      } else if(region == "south_fjords_odd") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Southern Fjords (odd)"), 
                      "exp_rate"]
      } else if(region == "east_vi") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "East Vancouver Island (odd)"), 
                      "exp_rate"]
      } else if(region == "hk") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Homathko-Klinaklini (odd)"), 
                      "exp_rate"]
      } else if(region == "nahwitti") {
        esc_df_short[row, "exp"] = 
          pink_area12[which(pink_area12$year == cur_year &
                              pink_area12$conservation_unit == 
                              "Nahwitti"), 
                      "exp_rate"]
      } else {
        stop("ERROR - some non-matching on row ", row)
      }
    } else { # if the current river is NOT in the helper df
      # take the mean of the values in that year (likely just one)
      esc_df_short[row, "exp"] = 
        mean(pink_area12[which(pink_area12$year == cur_year),
                       "exp_rate"]$exp_rate, 
           na.rm = TRUE)
    }
  }
}

## Set up overall S-R data base with all rivers ================================

# first check structure 
esc_df_short$esc = as.numeric(esc_df_short$esc)
esc_df_short[which(esc_df_short$esc == 0),"esc"] = NA

# Recruitment estimates R = N/(1-u)
esc_df_short = esc_df_short %>% 
  rowwise() %>% 
  mutate(R = esc/(1-exp))

esc_df_short$S = NA
esc_df_short$survival = NA

# Spawner estimates to pair with recruitment (S(t-2) corresponds to R(t))
esc_df_short$year = as.numeric(esc_df_short$year)
for(river in unique(esc_df_short$river)) {
  for(year in 1954:2016) {
    
    # check if the previous year is even there 
    if(nrow(esc_df_short[which(esc_df_short$river == river &
                               esc_df_short$year == (year - 2)),]) == 0){
      esc_df_short[which(esc_df_short$river == river & 
                           esc_df_short$year == year), "S"] = NA
      esc_df_short[which(esc_df_short$river == river & 
                           esc_df_short$year == year), "survival"] = NA
    } else {
      
      # assign the spawners
      esc_df_short[which(esc_df_short$river == river & 
                           esc_df_short$year == year), "S"] =
        esc_df_short[which(esc_df_short$river == river &
                             esc_df_short$year == (year - 2)), "esc"]
      # now assign survival 
      esc_df_short[which(esc_df_short$river == river & 
                           esc_df_short$year == year), "survival"] =
        #log(R(t)/S(t-2))
        (esc_df_short[which(esc_df_short$river == river & 
                              esc_df_short$year == year),"R"]) / 
        (esc_df_short[which(esc_df_short$river == river &
                              esc_df_short$year == year), "S"]) 
    }
  }
}

# add log survival 
esc_df_short$log_survival = log(esc_df_short$survival)

# Remove ambiguous farm exposure/enhancement/etc ===============================

# get the rivers to be included -- these come from Steph's work
area12_rivers = c("AHNUHATI RIVER", "AHTA RIVER", "GLENDALE CREEK", 
                  "KAKWEIKEN RIVER", "KINGCOME RIVER", "LULL CREEK", 
                  "VINER SOUND CREEK", "WAKEMAN RIVER")
area07_rivers = c("PINE RIVER", "NEEKAS CREEK", "TANKEEAH RIVER", 
                  "KWAKUSDIS RIVER", "BULLOCK CHANNEL CREEKS", "QUARTCHA CREEK", 
                  "LEE CREEK", "ROSCOE CREEK", "CLATSE CREEK", 
                  "WALKER LAKE CREEK", "GOAT BUSHU CREEK", 
                  "DEER PASS LAGOON CREEKS", "KUNSOOT RIVER", "KADJUSDIS RIVER", 
                  "MCLOUGHLIN CREEK", "COOPER INLET CREEKS")

# subset the dataframes 
area_12_df = esc_df_short[which(esc_df_short$area == 12),]
area_07_df = esc_df_short[which(esc_df_short$area == 7),]

# all other areas 
other_areas_df = esc_df_short[which(esc_df_short$area %notin% c(12, 7)),]

# filter to only the desired areas 
area_12_df = area_12_df %>% 
  dplyr::filter(river %in% area12_rivers)
area_07_df = area_07_df %>% 
  dplyr::filter(river %in% area07_rivers)

# bind all other areas and the filtered ones
new_esc_df = rbind(area_07_df, area_12_df, other_areas_df)

# specify even vs odd years ====================================================

new_esc_df = new_esc_df %>% 
  rowwise() %>% 
  dplyr::mutate(even_odd = ifelse(
    (year %% 2) == 0, "even",
    "odd"
  ))

# Only keep pop'ns with a minimum of 20 spawner-recruit pairs ==================

# first define populations as even/odd in the same river 
new_esc_df$pop = NA
num_rivers = unique(new_esc_df$river)

# iterate through rivers 
n_pops = 1
for(river in num_rivers) {
  
  # assign population the value for odd years
  new_esc_df$pop[which(new_esc_df$river == river 
                       & new_esc_df$even_odd == "odd")] = n_pops
  # iterate
  n_pops = n_pops + 1
  # now check even years 
  new_esc_df$pop[which(new_esc_df$river == river 
                       & new_esc_df$even_odd == "even")] = n_pops
  # iterate
  n_pops = n_pops + 1
}

# make this into a factor 
new_esc_df$pop = as.factor(new_esc_df$pop)

# loop through and figure out how many pairs there are in each populations
populations = sort(unique(new_esc_df$pop))
counts = numeric(length(unique(new_esc_df$pop)))
for(curr_pop in 1:length(unique(new_esc_df$pop))) {
  
  # grab the current df of the population we want 
  temp = new_esc_df[which(new_esc_df$pop == curr_pop),]
  
  # if there are enough mon-NA's then keep it
  # (I know this is ugly and slow i just wanted to make 100% sure i was doing
  # it out properly)
  n_rows = nrow(temp)
  n_NAs = nrow(temp[which(is.na(temp$survival)),])
  counts[curr_pop] = n_rows - n_NAs
  
}

pop_count_df = data.frame(
  population = populations,
  count = counts
)

enough_obs_df_20 = pop_count_df[which(pop_count_df$count >= 20),]
enough_obs_df_03 = pop_count_df[which(pop_count_df$count > 3),]


final_rivers_df_20 = new_esc_df[which(new_esc_df$pop %in% 
                                        enough_obs_df_20$population),]
final_rivers_df_03 = new_esc_df[which(new_esc_df$pop %in% 
                                        enough_obs_df_03$population),]

# name the populations
final_rivers_df_20$population_name = paste(
  stringr::str_to_lower(gsub(" ", "_", final_rivers_df_20$river)),
  final_rivers_df_20$even_odd,
  sep = "_"
)
final_rivers_df_03$population_name = paste(
  stringr::str_to_lower(gsub(" ", "_", final_rivers_df_03$river)),
  final_rivers_df_03$even_odd,
  sep = "_"
)

# remove NA observations of survival
final_rivers_df_20 = final_rivers_df_20 %>% 
  dplyr::filter(
    !is.na(survival)
  )

final_rivers_df_03 = final_rivers_df_03 %>% 
  dplyr::filter(
    !is.na(survival)
  )

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(final_rivers_df_20$pop)), "\n Total number of S-R pairs: ", 
    dim(final_rivers_df_20)[1], "\n Total number of rivers: ", 
    length(unique(final_rivers_df_20$river)))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(final_rivers_df_03$pop)), "\n Total number of S-R pairs: ", 
    dim(final_rivers_df_03)[1], "\n Total number of rivers: ", 
    length(unique(final_rivers_df_03$river)))


# bring in louse covariate =====================================================

final_rivers_df_20$lice = NA

# deal with all zero values first - non-area 12, and pre-2001
final_rivers_df_20[which(final_rivers_df_20$area != 12), "lice"] = 0
final_rivers_df_20[which(final_rivers_df_20$area == 12 & 
                        final_rivers_df_20$year < 2002), "lice"] = 0

# now do the area 12 that we can
for(yr in 2002:2017) {
  
  # get the subset 
  final_rivers_df_20[which(final_rivers_df_20$area == 12 & 
                          final_rivers_df_20$year == yr), "lice"] = 
    # find the value from the other dataset
    lice_pred[which(lice_pred$year == yr-1), "all_lep"]
    ## NOTE ###
    # the -1 in line above is supposed to be there, to pair the year of the lice
    # infection with the return year
    ## END NOTE ##
}

# repeat for all versions one 
final_rivers_df_03$lice = NA

# deal with all zero values first - non-area 12, and pre-2001
final_rivers_df_03[which(final_rivers_df_03$area != 12), "lice"] = 0
final_rivers_df_03[which(final_rivers_df_03$area == 12 & 
                   final_rivers_df_03$year < 2001), "lice"] = 0

# now do the area 12 that we can
for(yr in 2002:2017) {
  
  # get the subset 
  final_rivers_df_03[which(final_rivers_df_03$area == 12 & 
                             final_rivers_df_03$year == yr), "lice"] = 
    # find the value from the other dataset
    lice_pred[which(lice_pred$year == yr-1), "all_lep"]
  ## NOTE ###
  # the -1 in line above is supposed to be there, to pair the year of the lice
  # infection with the return year
  ## END NOTE ##
  
}

# make dataframe of number of rivers sampled per year/year through time
final_rivers_20_plot_df = data.frame(
  table(final_rivers_df_20[,c("year", "area")])
)
final_rivers_20_plot_df$year_num = 
  as.numeric(as.character(final_rivers_20_plot_df$year))
final_rivers_20_plot_df = final_rivers_20_plot_df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(`Even/Odd Year` = ifelse(year_num %% 2 == 0, "even", "odd")) %>% 
  rename(Area = area)
# check the length is correct
if(
  nrow(final_rivers_20_plot_df) != length(unique(final_rivers_df_20$year)) * 
  length(unique(final_rivers_df_20$area))
) {
  stop("ERROR - table of incorect length")
}

# make dataframe of number of rivers sampled per year/year through time
final_rivers_03_plot_df = data.frame(
  table(final_rivers_df_03[,c("year", "area")])
)
final_rivers_03_plot_df$year_num = 
  as.numeric(as.character(final_rivers_03_plot_df$year))
final_rivers_03_plot_df = final_rivers_03_plot_df %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(`Even/Odd Year` = ifelse(year_num %% 2 == 0, "even", "odd")) %>% 
  rename(Area = area)
# check the length is correct
if(
  nrow(final_rivers_03_plot_df) != length(unique(final_rivers_df_03$year)) * 
  length(unique(final_rivers_df_03$area))
) {
  stop("ERROR - table of incorect length")
}

# make plots of each
col_vals = rev(PNWColors::pnw_palette("Bay",
                                      type = "discrete", n = 5))

# 20 cutoff plot
final_rivers_20_plot = ggplot2::ggplot(data = final_rivers_20_plot_df) +
  geom_line(aes(x = year_num, y = Freq, colour = Area,
                linetype = `Even/Odd Year`, alpha = `Even/Odd Year`), 
            size = 1.5) + 
  geom_point(aes(x = year_num, y = Freq, fill = Area,
                 shape = `Even/Odd Year`), size = 3) +
  scale_alpha_manual(values = c(0.5, 0.4)) +
  scale_shape_manual(values = c(21,22)) +
  theme_area_grouping() +
  theme(
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(
    x = "Year", y = "Number of Rivers Surveyed"
  ) +
  scale_colour_manual(
    "Area",
    values = col_vals,
    labels = c(7, 8, 9, 10, 12)
  ) +
  scale_fill_manual(
    "Area",
    values = col_vals,
    labels = c(7, 8, 9, 10, 12)
  ) +
  annotate(
    geom = "text", x = 2002, y = 32, label = "Cutoff >= 20 S-R pairs", size = 6
  ) + 
  ylim(c(0,35)) +
  scale_x_continuous(
    labels = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
  ) +
  guides(
    fill = guide_legend(override.aes = list(shape = 21)),
    shape = guide_legend(override.aes = list(fill = "grey20"))
  ) +
  coord_fixed(2) +
  theme(
    legend.position = "none"
  )
ggplot2::ggsave(
  here::here("./figs/rivers-surveyed-through-time-20-pair-cutoff.png"),
  final_rivers_20_plot,
  height = 6, width = 10)

# 03 plot
final_rivers_03_plot = ggplot2::ggplot(data = final_rivers_03_plot_df) +
  geom_line(aes(x = year_num, y = Freq, colour = Area,
                linetype = `Even/Odd Year`, alpha = `Even/Odd Year`), 
            size = 1.5) + 
  geom_point(aes(x = year_num, y = Freq, fill = Area,
                 shape = `Even/Odd Year`), size = 3) +
  scale_alpha_manual(values = c(0.5, 0.4)) +
  scale_shape_manual(values = c(21,22)) +
  theme_area_grouping() +
  theme(
    legend.key.width = unit(1.5, "cm")
  ) +
  labs(
    x = "Year", y = ""
  ) +
  scale_colour_manual(
    "Area",
    values = col_vals,
    labels = c(7, 8, 9, 10, 12)
  ) +
  scale_fill_manual(
    "Area",
    values = col_vals,
    labels = c(7, 8, 9, 10, 12)
  ) +
  ylim(c(0,35)) +
  scale_x_continuous(
    labels = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
    breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)
  ) +
  annotate(
    geom = "text", x = 2002, y = 32, label = "Cutoff > 3 S-R pairs", size = 6
  ) + 
  guides(
    fill = guide_legend(override.aes = list(shape = 21)),
    shape = guide_legend(override.aes = list(fill = "grey20"))
  ) +
  coord_fixed(2) 
  # theme(
  #  legend.position = "none"
  # )
ggplot2::ggsave(
  here::here("./figs/rivers-surveyed-through-time-03-pair-cutoff.png"),
  final_rivers_03_plot,
  height = 6, width = 10)

# bind them together
cutoff_comp = final_rivers_20_plot + final_rivers_03_plot
ggplot2::ggsave(
  here::here("./figs/rivers-surveyed-through-time-comparison.png"),
  cutoff_comp,
  height = 6, width = 20)
# make final data base =========================================================
cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(final_rivers_df_20$pop)), "\n Total number of S-R pairs: ", 
    dim(final_rivers_df_20)[1], "\n Total number of rivers: ", 
    length(unique(final_rivers_df_20$river)))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(final_rivers_df_03$pop)), "\n Total number of S-R pairs: ", 
    dim(final_rivers_df_03)[1], "\n Total number of rivers: ", 
    length(unique(final_rivers_df_03$river)))

print(final_rivers_03_plot_df[which(final_rivers_03_plot_df$year_num == 2016), 
                               c("Area", "year_num", "Freq")])
print(final_rivers_20_plot_df[which(final_rivers_20_plot_df$year_num == 2016), 
                              c("Area", "year_num", "Freq")])

readr::write_csv(final_rivers_df_20, 
                 here::here(
                   "./data/for-model-runs/stock-recruit-data-cut-off-20.csv"))
readr::write_csv(final_rivers_df_03, 
                 here::here(
                   "./data/for-model-runs/stock-recruit-data-cut-off-03.csv"))
