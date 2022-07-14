##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-07-03
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(glmmTMB)
library(parallel)
library(PNWColors)

# pull in file with all functions to clean data 
source(here::here("./src/01_plot_themes.R"))

sr_df = readr::read_csv(here::here(
  "./data/for-model-runs/stock-recruit-data-cut-off-03.csv"
))
predict_df = readr::read_csv(here::here(
  "./data/regression-data/predicted-lice-abundance.csv"))

