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
library(lme4)

# pull in themes
source(here("./src/01_plot_themes.R"))

farm_regress = readr::read_csv(
    here("./data/regression-data/farm-regression-data.csv"))
scfs_regress = readr::read_csv(
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)
scfs_regress$year = as.factor(as.character(scfs_regress$year))
scfs_regress$farm = as.factor(as.character(scfs_regress$farm))
scfs_regress$week = as.factor(as.character(scfs_regress$week))

# clean farm data 
farm_regress$farm = as.factor(as.character(farm_regress$farm))
farm_regress$year = as.factor(as.character(farm_regress$year))

# read in model object
tmb_fit = readRDS(here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))

# model predictions ============================================================

predict_data = data.frame(year = as.character(c(2001:2021)),
                        week = NA,
                        farm = NA)
predict_data$all_lep = predict(tmb_fit, newdata = new_data,
            type = "response",
            re.form = NA
            )

# add in log data 
predict_data$log_all_lep = log10(predict_data$all_lep)

# add in farm data for comparison ==============================================

all_farms = farm_regress %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(leps = mean(lep_av))
focal_farms = farm_regress %>% 
    dplyr::filter(farm %in% c("Wicklow Point", "Burdwood", "Glacier Falls")) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(leps = mean(lep_av))

# compare between two farm groupings 
comp_data = data.frame(
    all_farms_measure = all_farms$leps,
    focal_farms_measure = focal_farms$leps
)
farm_grouping_regress = lm(all_farms_measure ~ focal_farms_measure,
                            data = comp_data)
summary(farm_grouping_regress)

ggplot(data = comp_data, aes(x = all_farms_measure, y = focal_farms_measure)) +
    geom_point(fill = ) + 
    geom_smooth(method = "lm") +
    labs(x = "All Farms", y = "Focal Farms") +
    theme_farm_grouping()

# plot regression
comp_data_years = data.frame(
    year = rep(all_farms$year, 2),
    value = c(comp_data$all_farms_measure, comp_data$focal_farms_measure),
    set = as.factor(c(rep("All Farms", nrow(comp_data)),
            rep("Focal Farms", nrow(comp_data)))
)
ggplot() + 
    geom_point(data = comp_data_years,
            aes(x = year, y = value, fill = set), 
                colour = "black",
                shape = 21,
                size = 2) + 
    geom_smooth(data = aes(x = allmethod=lm, se = TRUE)
