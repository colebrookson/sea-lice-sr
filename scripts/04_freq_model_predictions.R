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
library(PNWColors)
library(mgcv)
library(patchwork)

# pull in themes & some functions
source(here("./src/01_plot_themes.R"))
source(here("./src/02_data_cleaning_funs.R"))

farm_regress = read_csv(
    here("./data/clean-farm/marty-bati-data-joined-stocked-only.csv"))
scfs_regress = readr::read_csv(
    here("./data/regression-data/scfs-regression-leps-include-chals-data.csv"))

# ensure all counts are integer valued
scfs_regress$all_lep = as.integer(scfs_regress$all_lep)
scfs_regress$all_cal = as.integer(scfs_regress$all_cal)
scfs_regress$all_lice = as.integer(scfs_regress$all_lice)
scfs_regress$year = as.factor(as.character(scfs_regress$year))
scfs_regress$farm_name = as.factor(as.character(scfs_regress$farm_name))
scfs_regress$week = as.factor(as.character(scfs_regress$week))

# clean farm data 
farm_regress$farm_name = as.factor(as.character(farm_regress$farm_name)) 
farm_regress$year = as.factor(as.character(farm_regress$year))

# read in model object
tmb_fit = readRDS(here("./data/model-outputs/tmb-glmm-ap1-nb.RDS"))

# model predictions ============================================================

predict_data = data.frame(year = as.character(c(2001:2021)),
                        week = NA,
                        farm = NA)
predict_data$all_lep = predict(tmb_fit, newdata = predict_data,
            type = "response",
            re.form = NA,
            )

# add in log data 
predict_data$log_all_lep = log10(predict_data$all_lep)

# plot of model predictions
just_wild_timeline = ggplot(data = predict_data) +
    geom_point(aes(x = year, y = all_lep, fill = all_lep), 
                shape = 21, 
                colour = "black",
                size = 3.5) + 
    stat_smooth(aes(x = year, y = all_lep),
                    colour = "black") +
    labs(x = "Year", y = "Number of Lice per Fish", title = "Wild Leps") +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) +
    scale_x_discrete(breaks = c(2005, 2010, 2015, 2020)) +
    theme_raw_comp()
ggsave(filename = here("./figs/just-wild-raw-comparison.png"),
        plot = just_wild_timeline,
        width = 8,
        height = 6,
        dpi = 600)


# add in farm data for comparison ==============================================

all_farms = farm_regress %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(all_leps = mean(lep_tot))

ktc_farms = farm_regress %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(ktc == "Knight Tribune Corridor") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(ktc_leps = mean(lep_tot))

h_s_d_farms_df = farm_regress %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(hump_sarg_doc == "Humphrey-Sargeaunt-Doctors Triangle") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(hsd_leps = mean(lep_tot))

# compare between two farm groupings 
comp_data = data.frame(year = as.factor(c(2000:2021)))
comp_data_all = left_join(
    x = comp_data,
    y = all_farms,
    by = "year"
)
comp_data_all_ktc = left_join(
    x = comp_data_all,
    y = ktc_farms,
    by = "year"
)
comp_data_all_ktc_hsd = left_join(
    x = comp_data_all_ktc,
    y = h_s_d_farms_df,
    by = "year"
)
all_comp = left_join(
    x = comp_data_all_ktc_hsd,
    y = (predict_data %>% select(year, all_lep)),
    by = "year"
)

wild_to_all_farms =  mgcv::gam(all_lep ~ s(all_leps),
                        data = all_comp)
summary(wild_to_all_farms)
saveRDS(wild_to_all_farms, 
    here("./data/model-outputs/wild-lice-to-all-farms-gam.RDS"))
wild_to_ktc_farms = mgcv::gam(all_lep ~ s(ktc_leps),
                        data = all_comp)
summary(wild_to_ktc_farms)
saveRDS(wild_to_ktc_farms, 
    here("./data/model-outputs/wild-lice-to-ktc-farms-gam.RDS"))
wild_to_hsd_farms = mgcv::gam(all_lep ~ s(hsd_leps),
                        data = all_comp)
summary(wild_to_hsd_farms)
saveRDS(wild_to_hsd_farms, 
    here("./data/model-outputs/wild-lice-to-hsd-farms-gam.RDS"))


all_comp$log_ktc = log10(all_comp$ktc_leps)
all_comp$log_all_lep = log10(all_comp$all_lep)
plot(x = all_comp$log_ktc, y = all_comp$log_all_lep)










farm_grouping_regress = stats::lm(all_farms_measure ~ focal_farms_measure,
                            data = comp_data)
saveRDS(farm_grouping_regress, 
    here("./data/model-outputs/farm-grouping-comparisons.RDS"))
summary(farm_grouping_regress)

# Compare farm and wild data with models =======================================

library(tidyverse)
library(here)
library(glmmTMB)
library(lme4)
library(PNWColors)
library(mgcv)
library(patchwork)

# pull in themes & some functions
source(here("./src/01_plot_themes.R"))
source(here("./src/02_data_cleaning_funs.R"))

farm_regress = readr::read_csv(
    here("./data/regression-data/farm-regression-data.csv"))
predict_data$all_farms = c(NA, NA, all_farms$leps)
predict_data$focal_farms = c(NA, NA, focal_farms$leps)
predict_data$log_all_farms = log10(predict_data$all_farms)
predict_data$log_focal_farms = log10(predict_data$focal_farms)

# exclude years no farm data for 
predict_data = predict_data %>% 
    dplyr::filter(year %notin% c("2001", "2002"))

# write all sets of models
wild_to_all_farms =  mgcv::gam(all_lep ~ s(all_farms),
                        data = predict_data)
summary(wild_to_all_farms)
saveRDS(wild_to_all_farms, 
    here("./data/model-outputs/wild-lice-to-all-farms-gam.RDS"))
wild_to_focal_farms =  mgcv::gam(all_lep ~ s(focal_farms),
                        data = predict_data)
summary(wild_to_focal_farms)
saveRDS(wild_to_focal_farms, 
    here("./data/model-outputs/wild-lice-to-focal-farms-gam.RDS"))
log_wild_to_log_all_farms = stats::lm(log_all_lep ~ log_all_farms,
                        data = predict_data)
summary(log_wild_to_log_all_farms)
saveRDS(log_wild_to_log_all_farms, 
    here("./data/model-outputs/log-wild-lice-to-log-all-farms-lm.RDS"))
log_wild_to_log_focal_farms = stats::lm(log_all_lep ~ log_focal_farms,
                        data = predict_data)
summary(log_wild_to_log_focal_farms)
saveRDS(log_wild_to_log_focal_farms, 
    here("./data/model-outputs/log-wild-lice-to-log-focal-farms-lm.RDS"))

# do some data wrangling 
predict_data_long = data.frame(
    year = as.numeric(rep(predict_data$year, 6)),
    measure = c(rep("Wild Leps", 19),
                rep("Wild Leps (Log10)", 19),
                rep("All Farm Leps", 19),
                rep("All Farm Leps (Log10)", 19),
                rep("Focal Farm Leps", 19),
                rep("Focal Farm Leps (Log10)", 19)),
    value = c(predict_data$all_lep, 
              predict_data$log_all_lep,
              predict_data$all_farms,
              predict_data$log_all_farms,
              predict_data$focal_farms,
              predict_data$log_focal_farms)
)
# reorder the factor
predict_data_long$measure = factor(predict_data_long$measure, 
    levels = c("Wild Leps",
                       "All Farm Leps",
                       "Focal Farm Leps",
                       "Wild Leps (Log10)",
                       "All Farm Leps (Log10)",
                       "Focal Farm Leps (Log10)"))
# cut out the log values for this plot 
predict_data_long = predict_data_long %>% 
    filter(measure %in% c("Wild Leps",
                            "All Farm Leps",
                            "Focal Farm Leps"))
predict_data_long_wild = predict_data_long %>% 
    filter(measure == "Wild Leps")

# plots ========================================================================
summary(wild_to_ktc_farms)
summary(wild_to_hsd_farms)
summary(wild_to_all_farms)

p1 = ggplot(data = all_comp, 
        aes(x = all_leps, y = all_lep, fill = all_lep)) +
    geom_point(
        shape = 21,
        colour = "black",
        size = 4) + 
    stat_smooth(method = mgcv::gam, formula = y ~ s(x)) +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) + 
    labs(x = "Lice on All Farms", 
            y = "Lice on Wild Fish", 
            title = "All Farms") + 
    theme_mod_comp() + 
    annotate(geom = "text", 
                x = 3.5, 
                y = 1.0, 
                label = paste("R^2 ==", 0.753), 
                size = 7,
                parse = TRUE)

p2 = ggplot(data = all_comp, 
        aes(x = ktc_leps, y = all_lep, fill = all_lep)) +
    geom_point(
        shape = 21,
        colour = "black",
        size = 4) + 
    stat_smooth(method = mgcv::gam, formula = y ~ s(x)) +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) + 
    labs(x = "Lice on KTC Farms", 
            y = "Lice on Wild Fish", 
            title = "Focal Farms") + 
    theme_mod_comp() + 
    annotate(geom = "text", 
                x = 2.75, 
                y = 1.0, 
                label = paste("R^2 ==", 0.872), 
                size = 7,
                parse = TRUE)
p3 = ggplot(data = predict_data, 
        aes(x = log_all_farms, y = log_all_lep, fill = log_all_lep)) +
    geom_point(
        shape = 21,
        colour = "black",
        size = 4) + 
    stat_smooth(method = stats::lm, formula = y ~ x) +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) + 
    labs(x = "Lice on All Farms (Log 10)", 
            y = "Lice on Wild Fish (Log 10)", 
            title = "All Farms (Log 10)") + 
    theme_mod_comp() + 
    annotate(geom = "text", 
                x = 0.4, 
                y = 0.75, 
                label = paste("R^2 ==", 0.114), 
                size = 7,
                parse = TRUE)
p4 = ggplot(data = predict_data, 
        aes(x = log_focal_farms, y = log_all_lep, fill = log_all_lep)) +
    geom_point(
        shape = 21,
        colour = "black",
        size = 4) + 
    stat_smooth(method = stats::lm, formula = y ~ x) +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) + 
    labs(x = "Lice on Focal Farms (Log 10)", 
            y = "Lice on Wild Fish (Log 10)", 
            title = "Focal Farms (Log 10)") + 
    theme_mod_comp() + 
    annotate(geom = "text", 
                x = 0.4, 
                y = 0.75, 
                label = paste("R^2 ==", -0.038), 
                size = 7,
                parse = TRUE)

p_all = (p1 + p2) / (p3 + p4)
ggsave(filename = here("./figs/wild-to-farm-models-comparison.png"),
        plot = p_all,
        width = 15,
        height = 15,
        dpi = 600)
raw_data_timeline = ggplot(data = predict_data_long) +
    geom_point(aes(x = year, y = value, fill = value), 
                shape = 21, 
                colour = "black",
                size = 3.5) + 
    stat_smooth(aes(x = year, y = value),
                    colour = "black") +
    facet_wrap(~measure,
        nrow = 2, ncol = 3, scales = "fixed") +
    labs(x = "Year", y = "Number of Lice per Fish") +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) +
    theme_raw_comp()
ggsave(filename = here("./figs/wild-to-farm-raw-data-comparison.png"),
        plot = raw_data_timeline,
        width = 20,
        height = 8,
        dpi = 600)











# make and save plots ==========================================================

farm_grouping_comp = ggplot(data = comp_data, 
        aes(x = all_farms_measure, y = focal_farms_measure)) +
    geom_point(aes(fill = all_farms_measure), 
                shape = 21, 
                colour = "black",
                size = 3.5) + 
    geom_smooth(method = "lm",
                colour = "black",
                size = 2,
                linetype = "dashed") +
    labs(x = "All Farms", y = "Focal Farms") +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) +
    theme_farm_grouping() + 
    annotate(geom = "text", 
                x = 3.5, 
                y = 3.9, 
                label = paste("R^2 ==", 0.535), 
                size = 7,
                parse = TRUE)
ggsave(filename = here("./figs/farm-grouping-comparison.png"),
        plot = farm_grouping_comp,
        width = 10,
        height = 8,
        dpi = 600)


