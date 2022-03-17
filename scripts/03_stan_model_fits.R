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
library(rstanarm)
library(glmmTMB)

farm_regress = read_csv(here("./data/regression-data/farm-regression-data.csv"))
scfs_regress = read_csv(here("./data/regression-data/scfs-regression-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)
scfs_regress$year = as.factor(as.character(scfs_regress$year))
scfs_regress$week = as.factor(as.character(scfs_regress$week))
scfs_regress$farm = as.factor(as.character(scfs_regress$farm))

# regresion ====================================================================

# perform the regression for all of all lice, all leps and all cals 
stan_glm_poi = stan_glmer(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress, family = poisson, 
                    prior = normal(0, 2.5),
                    prior_intercept = normal(0, 5),
                    seed = 1234)
saveRDS(stan_glm_poi, here("./data/model-outputs/stan-glm-poisson.RDS"))

stan_glm_nb = stan_glmer(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress, family = neg_binomial_2, 
                    prior = normal(0, 2.5),
                    prior_intercept = normal(0, 5),
                    seed = 1234)
saveRDS(stan_glm_nb, here("./data/model-outputs/stan-glm-nbinom.RDS"))

# read in objects
stan_glm_poi = readRDS(here("./data/model-outputs/stan-glm-poisson.RDS"))
stan_glm_nb = readRDS(here("./data/model-outputs/stan-glm-nbinom.RDS"))
plot(stan_glm_poi, plotfun = "trace")

# use glmmTMB to do this as well ===============================================

# fit models using the different approaches 
tmb_glmm_poi = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm), 
                    data = scfs_regress, 
                    family = poisson,
                    ziformula =  ~0
)
tmb_glmm_zip = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm), 
                    data = scfs_regress, 
                    family = poisson,
                    ziformula =  ~1
)
tmb_glmm_nb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm), 
                    data = scfs_regress, 
                    family = nbinom2,
                    ziformula =  ~0
)
tmb_glmm_zinb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm), 
                    data = scfs_regress, 
                    family = nbinom2,
                    ziformula =  ~1
)
str(scfs_regress)
