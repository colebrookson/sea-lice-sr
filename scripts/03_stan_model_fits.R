########## 
##########
# Pull in all data for cleaning 
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-02-02
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(bayesplot)
library(rstan)

farm_regress = read_csv(here("./data/regression-data/farm-regression-data.csv"))
scfs_regress = read_csv(here("./data/regression-data/scfs-regression-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)

# regresion ====================================================================

# perform the regression for all of all lice, all leps and all cals 
stan_glm_poi = stan_glm(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress, family = poisson, 
                    prior = normal(0, 2.5),
                    prior_intercept = normal(0, 5),
                    seed = 1234)
