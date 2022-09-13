########## 
##########
# All functions for fitting frequentist models to the count data
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-24
##########
##########

options(dplyr.summarise.inform = FALSE)

library(here)
library(tidyverse)

farm_df = read_csv(here("./data/wild-lice-data/clean/scfs-data-clean.csv"))
