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

# read in model object
tmb_fit = readRDS(here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))

summary(scfs_regress$year)

new_data = data.frame(year = as.character(c(2001:2021)),
                        week = NA,
                        farm = NA)
new_data$all_lep = predict(tmb_fit, newdata = new_data,
            type = "response",
            re.form = NA
            )
plot(new_data$year, new_data$all_lep)
