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
library(lme4)

# pull in file with all functions to clean data 
source(here::here("./src/01_plot_themes.R"))

sr_df = readr::read_csv(here::here(
  "./data/for-model-runs/stock-recruit-data-cut-off-03.csv"
))
predict_df = readr::read_csv(here::here(
  "./data/regression-data/predicted-lice-abundance.csv"))

alt_model = readRDS(here::here(
  "./data/model-outputs/stock-recruit-alternative-model.RDS"
))

# get effect size for all fixed effects ========================================

# using essentially hedges D = estimate / sqrt of sum of variances of random 
# effects 
variances = c(print(lme4::VarCorr(alt_model), 
                  comp = "Variance")$`area:year_fac`[[1]],
              print(lme4::VarCorr(alt_model), 
                    comp = "Variance")$`year_fac`[[1]])
sqrt_sum_var = sqrt(sum(variances))
fixed_effects = lme4::fixef(alt_model)

# get effect sizes 
effect_sizes = data.frame(fixed_effects/sqrt_sum_var)
effect_sizes$names = rownames(effect_sizes)
names(effect_sizes)[1] = "effect_size"
rownames(effect_sizes) = NULL



