# load =========================================================================

library(tidyverse)

## read nuseds
sr_data <- readr::read_csv(here::here(
  "./data/sr-data/NCC_streams_river-level_SR_data_2023-08-05.csv"),
  show_col_types = FALSE,
  guess_max = 1000000) %>% 
  standardize_names()


head(sr_data)
names(sr_data)
unique(sr_data$species)

# make sure all variables are the type they need to be in the model 
sr_data$river <- as.factor(sr_data$river)
sr_data$broodyear <- as.factor(sr_data$broodyear)
sr_data$statarea <- as.factor(sr_data$statarea)

# pinks processing =============================================================
pinks_sr <- sr_data %>% 
  dplyr::filter(species %in% c("PK)", "PKE"))

null_model_pink <- lme4::lmer()

# chum processing ==============================================================
chum_sr <- sr_data %>% 
  dplyr::filter(species == "CM")
