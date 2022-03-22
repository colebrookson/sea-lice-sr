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