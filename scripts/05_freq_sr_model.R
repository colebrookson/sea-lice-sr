##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-06-03
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(glmmTMB)

# pull in file with all functions to clean data 
source(here::here("./src/01_plot_themes.R"))

sr_df = readr::read_csv(here::here(
  "./data/for-model-runs/stock-recruit-data.csv"
))
unfilter_sr_df= readr::read_csv(here::here(
  "./data/for-model-runs/non-filtered-stock-recruit-data.csv"
))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(sr_df)[1], "\n Total number of rivers: ", 
    length(unique(sr_df$river)))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(unfilter_sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(unfilter_sr_df)[1], "\n Total number of rivers: ", 
    length(unique(unfilter_sr_df$river)))
# last touch-up's of the data ==================================================

sr_df[which(sr_df$area == 12 & year %in% c(1990:2000)), "lice"] = NA
sr_df = sr_df[which(sr_df$year > 1961),]

sr_df$area = as.factor(sr_df$area)
sr_df$year_fac = as.factor(sr_df$year)
sr_df$population_name = as.factor(sr_df$population_name)

# do it again for unfiltered data 
unfilter_sr_df[which(unfilter_sr_df$area == 12 & 
                         year %in% c(1990:2000)), "lice"] = NA
unfilter_sr_df = unfilter_sr_df[which(unfilter_sr_df$year > 1961),]

unfilter_sr_df$area = as.factor(unfilter_sr_df$area)
unfilter_sr_df$year_fac = as.factor(unfilter_sr_df$year)
unfilter_sr_df$population_name = as.factor(unfilter_sr_df$population_name)

# run models ===================================================================

null_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                (1|year_fac/area),
                              data = sr_df)
saveRDS(null_model, here::here(
  "./data/model-outputs/stock-recruit-null-model.RDS"
))
alt_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                               scale(lice) + (1|year_fac/area),
                             data = sr_df)
saveRDS(alt_model, here::here(
  "./data/model-outputs/stock-recruit-alternative-model.RDS"
))

summary(null_model)
summary(alt_model)
anova(null_model, alt_model)

cat("value of fitted r (growth rate) is ", alt_model$fit$par[[1]],
      " and the value of c (effect of sea lice) is ", alt_model$fit$par[[2]])

# attempt to fit models with observations not excluded 

unfilter_null_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                (1|year_fac/area),
                              data = unfilter_sr_df)
saveRDS(unfilter_null_model, here::here(
  "./data/model-outputs/unfiltered-data-stock-recruit-null-model.RDS"
))
unfilter_alt_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                        scale(lice) + 
                                        (1|year_fac/area),
                                      data = unfilter_sr_df)
saveRDS(alt_model, here::here(
  "./data/model-outputs/unfiltered-data-stock-recruit-alternative-model.RDS"
))

summary(unfilter_null_model)
summary(unfilter_alt_model)
anova(unfilter_null_model, unfilter_alt_model)

