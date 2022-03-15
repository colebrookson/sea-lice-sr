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

