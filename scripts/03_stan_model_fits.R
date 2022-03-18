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
scfs_regress = read_csv(
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)
scfs_regress$year = as.factor(as.character(scfs_regress$year))
scfs_regress$farm_year = as.factor(as.character(scfs_regress$farm_year))
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
tmb_glmm_poi = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm_year),
                    data = scfs_regress,
                    family = poisson,
                    ziformula =  ~0
)
saveRDS(tmb_glmm_poi, here(".data/model-outputs/tmb-glmm-poi.RDS"))
tmb_glmm_zip = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm_year),
                    data = scfs_regress,
                    family = poisson,
                    ziformula =  ~1
)
saveRDS(tmb_glmm_zip, here(".data/model-outputs/tmb-glmm-zip.RDS"))
tmb_glmm_nb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm_year),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0
)
saveRDS(tmb_glmm_nb, here(".data/model-outputs/tmb-glmm-nb.RDS"))
tmb_glmm_zinb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm_year),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~1
)
saveRDS(tmb_glmm_zinb, here(".data/model-outputs/tmb-glmm-zinb.RDS"))


AIC(tmb_glmm_poi, tmb_glmm_zip, tmb_glmm_nb, tmb_glmm_zinb)

# notes
# random is for week -- farm could be included as a fixed effect,
# could just compare the results - if the mean effect is different between them,
# 
# if you include the chalimus around, it would have to be done on a year/site 
# ratio. Essentially ONLY within the sampling event - because that matters 
# within a sampling event 