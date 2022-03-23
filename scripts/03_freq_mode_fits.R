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
library(glmmTMB)
library(DHARMa)
library(lme4)

farm_regress = read_csv(here("./data/regression-data/farm-regression-data.csv"))
scfs_regress = read_csv(
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)
scfs_regress$year = as.factor(as.character(scfs_regress$year))
scfs_regress$farm = as.factor(as.character(scfs_regress$farm))
scfs_regress$week = as.factor(as.character(scfs_regress$week))

# use glmmTMB ==================================================================

# fit models using the different approaches 

# approach 1 - location as random effect
tmb_glmm_1_nb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS"))
)
saveRDS(tmb_glmm_1_nb, here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))
tmb_glmm_1_zinb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~1,
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS"))
)
saveRDS(tmb_glmm_1_zinb, here("./data/model-outputs/tmb-glmm-ap1-zinb.RDS"))

# aproach 2 - location as fixed effect
tmb_glmm_2_nb = glmmTMB(all_lep ~ year + farm + (1 | week),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS"))
)
saveRDS(tmb_glmm_2_nb, here("./data/model-outputs/tmb-glmm-ap2-nb.RDS"))
tmb_glmm_2_zinb = glmmTMB(all_lep ~ year + farm + (1 | week),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~1,
                    control = glmmTMBControl(optimizer = optim, 
                                             optArgs = list(method = "BFGS"))
)
saveRDS(tmb_glmm_2_zinb, here("./data/model-outputs/tmb-glmm-ap2-zinb.RDS"))

AIC(tmb_glmm_1_nb, tmb_glmm_1_zinb, tmb_glmm_2_nb, tmb_glmm_2_zinb)

# read in and inspect model objects ============================================

# read in model objects
tmb_1 = readRDS(here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))
tmb_2 = readRDS(here("./data/model-outputs/tmb-glmm-ap2-nb.RDS"))

# simulate residuals
tmb_1_simres = simulateResiduals(tmb_1)
tmb_2_simres = simulateResiduals(tmb_2)

# plot residuals 
plot(tmb_1_simres)
plot(tmb_2_simres)

basic = glmmTMB(all_lep ~ year,
                data = scfs_regress,
                family = nbinom2,
                control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))