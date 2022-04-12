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

farm_regress = read_csv(
    here("./data/clean-farm/marty-bati-data-joined-stocked-only.csv"))
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

# set up parallel fitting option
n_cores = min(parallel::detectCores(), 8)

# fit models using the different approaches 

# approach 1 - location as random effect
tmb_glmm_1_nb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS"),
                                             parallel = n_cores)
)
saveRDS(tmb_glmm_1_nb, here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))
tmb_glmm_1_zinb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~1,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS"),
                                             parallel = n_cores)
)
saveRDS(tmb_glmm_1_zinb, here("./data/model-outputs/tmb-glmm-ap1-zinb.RDS"))

# aproach 2 - location as fixed effect
tmb_glmm_2_nb = glmmTMB(all_lep ~ year + farm + (1 | week),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS"),
                                             parallel = n_cores)
)
saveRDS(tmb_glmm_2_nb, here("./data/model-outputs/tmb-glmm-ap2-nb.RDS"))
tmb_glmm_2_zinb = glmmTMB(all_lep ~ year + farm + (1 | week),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~1,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "BFGS"),
                                             parallel = n_cores)
)
saveRDS(tmb_glmm_2_zinb, here("./data/model-outputs/tmb-glmm-ap2-zinb.RDS"))

# check AIC values
AIC(tmb_glmm_1_nb, tmb_glmm_1_zinb, tmb_glmm_2_nb, tmb_glmm_2_zinb)

## BEGIN NOTE ###############################
# So we see that the best models for both the approach 1 and approach 2 options 
# are the non-zero inflated options (probably since it's a type II negative 
# binomial), so we'll stick with those for now. IMPORTANTLY, the difference in 
# AIC between approach 1) and 2) isn't that large (~10 AIC points) so the 
# previously-used method will be retained.
## END NOTE ###############################

# compare methods for optimization

# default method is the one from above - quasi-Newtonian
tmb_bfgs = tmb_glmm_1_nb

# CG method is conjugate gradient method 
tmb_cg = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(method = "CG"),
                                             parallel = n_cores)
)
saveRDS(tmb_cg, here("./data/model-outputs/tmb-cg.RDS"))

# L-BFGS-B is a limited-memory modification of the BFGS quasi-Newtonian method
tmb_lbfgsb = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(
                                                 method = "L-BFGS-B"),
                                             parallel = n_cores)
)
saveRDS(tmb_lbfgsb, here("./data/model-outputs/tmb-lbfgsb.RDS"))

# SANN is a version of simulated annealing 
tmb_sann = glmmTMB(all_lep ~ year + (1 | week) + (1 | farm),
                    data = scfs_regress,
                    family = nbinom2,
                    ziformula =  ~0,
                    control = glmmTMBControl(optimizer = optim,
                                             optArgs = list(
                                                 method = "SANN"),
                                             parallel = 10)
)
saveRDS(tmb_sann, here("./data/model-outputs/tmb-sann.RDS"))

tmb_bfgs = readRDS(here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))
tmb_cg = readRDS(here("./data/model-outputs/tmb-cg.RDS"))
tmb_lbfgsb = readRDS(here("./data/model-outputs/tmb-lbfgsb.RDS"))
tmb_sann = readRDS(here("./data/model-outputs/tmb-sann.RDS"))

AIC(tmb_bfgs, tmb_cg, tmb_lbfgsb, tmb_sann)

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
