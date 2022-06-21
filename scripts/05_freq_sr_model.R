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

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(sr_df)[1], "\n Total number of rivers: ", 
    length(unique(sr_df$river)))
last touch-up's of the data ==================================================

sr_df[which(sr_df$area == 12 & year %in% c(1990:2000)), "lice"] = NA
sr_df = sr_df[which(sr_df$year > 1961),]


sr_df$area = as.factor(sr_df$area)
sr_df$year_fac = as.factor(sr_df$year)
sr_df$population_name = as.factor(sr_df$population_name)


# run models ===================================================================
null_lme = lmer(log_survival ~ scale(S):population_name + 
                  (1|year_fac/area), 
                data = sr_df)
alt_lme = lmer(log_survival ~ scale(S):population_name + 
                 scale(lice) + (1|year_fac/area), 
               data = sr_df)

anova(null_lme, alt_lme)



# null_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name + 
#                                 (1|year_fac/area), 
#                               data = sr_df)
# alt_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name + 
#                                scale(lice) + (1|year_fac/area), 
#                              data = sr_df)

summary(null_model)
summary(alt_model)
anova(null_model, alt_model)


