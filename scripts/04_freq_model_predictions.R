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
source(here::here("./src/01_plot_themes.R"))
source(here::here("./src/02_data_cleaning_funs.R"))

farm_regress = read_csv(
    here::here("./data/farm-data/clean/marty-bati-data-joined-stocked-only.csv"))
scfs_regress = readr::read_csv(
    here::here("./data/prepped-data/scfs-regression-leps-include-chals-data.csv"))

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
tmb_fit = readRDS(here::here("./outputs/model-outputs/tmb-glmm-ap1-nb.RDS"))
summary(tmb_fit)

scfs_regress %>% 
    dplyr::group_by(year) %>% 
    summarize(mean_leps = mean(all_lep))
# model predictions ============================================================

predict_data = data.frame(year = as.character(c(2001:2021)),
                        week = NA,
                        farm_name = NA)
all_lep = data.frame(predict(tmb_fit, newdata = predict_data,
            type = "response",
            re.form = NA,
            se.fit = TRUE
            ))
predict_data$all_lep = all_lep$fit
predict_data$lower = all_lep$fit - (1.96*all_lep$se.fit)
predict_data$upper = all_lep$fit + (1.96*all_lep$se.fit)

# add in log data 
predict_data$log_all_lep = log10(predict_data$all_lep)

# plot of model predictions
just_wild_timeline = ggplot(data = predict_data) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0) +
    geom_point(aes(x = year, y = all_lep, fill = all_lep), 
                shape = 21, 
                colour = "black",
                size = 4.5) + 
    stat_smooth(aes(x = year, y = all_lep),
                    colour = "black") +
    labs(x = "Year", y = "Number of Lice per Fish") +
    scale_fill_gradientn(
        colours = rev(PNWColors::pnw_palette("Sunset2",
                                        type = "continuous"))) +
    #scale_x_discrete(breaks = c(2005, 2010, 2015, 2020)) +
    theme_raw_comp() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )
ggsave(filename = here::here("./figs/just-wild-raw-comparison.png"),
        plot = just_wild_timeline,
        width = 8,
        height = 6,
        dpi = 600)

# write df 
readr::write_csv(predict_data, 
                 here::here(
                   "./data/prepped-data/predicted-lice-abundance.csv"))

# add in farm data for comparison ==============================================

all_farms = farm_regress %>% 
    dplyr::filter(year != "2000") %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(all_leps = mean(lep_tot, na.rm = TRUE))

ktc_farms = farm_regress %>% 
    dplyr::filter(year != "2000") %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(ktc == "Knight Tribune Corridor") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(ktc_leps = mean(lep_tot, na.rm = TRUE))
unique(farm_regress$farm_name)
h_s_d_farms_df = farm_regress %>% 
    dplyr::filter(year != "2000") %>% 
    dplyr::filter(month %in% c(3, 4)) %>% 
    dplyr::filter(hump_sarg_doc == "Humphrey-Sargeaunt-Doctors Triangle") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(hsd_leps = mean(lep_tot, na.rm = TRUE))

# compare between two farm groupings 
comp_data = data.frame(year = as.factor(c(2001:2021)))
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
    here::here("./outputs/model-outputs/wild-lice-to-all-farms-gam.RDS"))
wild_to_ktc_farms = mgcv::gam(all_lep ~ s(ktc_leps),
                        data = all_comp)
summary(wild_to_ktc_farms)
saveRDS(wild_to_ktc_farms, 
    here::here("./outputs/model-outputs/wild-lice-to-ktc-farms-gam.RDS"))
wild_to_hsd_farms = mgcv::gam(all_lep ~ s(hsd_leps),
                        data = all_comp)
summary(wild_to_hsd_farms)
saveRDS(wild_to_hsd_farms, 
    here::here("./outputs/model-outputs/wild-lice-to-hsd-farms-gam.RDS"))

all_comp$log_ktc = log10(all_comp$ktc_leps)
all_comp$log_all_lep = log10(all_comp$all_lep)
plot(x = all_comp$log_ktc, y = all_comp$log_all_lep)

# farm_grouping_regress = stats::lm(all_farms_measure ~ focal_farms_measure,
#                             data = comp_data)
# saveRDS(farm_grouping_regress, 
#     here::here("./outputs/model-outputs/farm-grouping-comparisons.RDS"))
# summary(farm_grouping_regress)

# Compare farm and wild data with models =======================================

predict_data$all_farms = all_comp$all_leps
predict_data$log_all_farms = log10(all_comp$all_leps)
predict_data$ktc_leps = all_comp$ktc_leps
predict_data$hsd_leps = all_comp$hsd_leps
predict_data$log_ktc_leps = log10(all_comp$ktc_leps)
predict_data$log_hsd_leps = log10(all_comp$hsd_leps)

# write all sets of models
wild_to_all_farms =  mgcv::gam(all_lep ~ s(all_farms),
                        data = predict_data)
summary(wild_to_all_farms)
saveRDS(wild_to_all_farms, 
    here::here("./outputs/model-outputs/wild-lice-to-all-farms-gam.RDS"))

wild_to_ktc_farms =  mgcv::gam(all_lep ~ s(ktc_leps),
                        data = predict_data)
summary(wild_to_ktc_farms)
saveRDS(wild_to_ktc_farms, 
    here::here("./outputs/model-outputs/wild-lice-to-ktc-farms-gam.RDS"))

wild_to_hsd_farms =  mgcv::gam(all_lep ~ s(hsd_leps),
                               data = predict_data)
summary(wild_to_hsd_farms)
saveRDS(wild_to_hsd_farms, 
        here::here("./outputs/model-outputs/wild-lice-to-hsd-farms-gam.RDS"))

log_wild_to_log_all_farms = stats::lm(log_all_lep ~ log_all_farms,
                        data = predict_data)
summary(log_wild_to_log_all_farms)
saveRDS(log_wild_to_log_all_farms, 
    here::here("./outputs/model-outputs/log-wild-lice-to-log-all-farms-lm.RDS"))

log_wild_to_log_ktc_farms = stats::lm(log_all_lep ~ log_ktc_leps,
                        data = predict_data)
summary(log_wild_to_log_ktc_farms)
saveRDS(log_wild_to_log_ktc_farms, 
    here::here("./outputs/model-outputs/log-wild-lice-to-log-ktc-farms-lm.RDS"))

log_wild_to_log_hsd_farms = stats::lm(log_all_lep ~ log_hsd_leps,
                                      data = predict_data)
summary(log_wild_to_log_hsd_farms)
saveRDS(log_wild_to_log_hsd_farms, 
        here::here("./outputs/model-outputs/log-wild-lice-to-log-hsd-farms-lm.RDS"))

# do some data wrangling 
predict_data_long = data.frame(
    year = as.numeric(rep(predict_data$year, 8)),
    measure = c(rep("Wild Leps", 21),
                rep("Wild Leps (Log10)", 21),
                rep("All Farm Leps", 21),
                rep("All Farm Leps (Log10)", 21),
                rep("KTC Farm Leps", 21),
                rep("KTC Farm Leps (Log10)", 21),
                rep("HSD Farm Leps", 21),
                rep("HSD Farm Leps (Log10)", 21)),
    value = c(predict_data$all_lep, 
              predict_data$log_all_lep,
              predict_data$all_farms,
              predict_data$log_all_farms,
              predict_data$ktc_leps,
              predict_data$log_ktc_leps,
              predict_data$hsd_leps,
              predict_data$log_hsd_leps)
)
# reorder the factor
predict_data_long$measure = factor(predict_data_long$measure, 
    levels = c("Wild Leps", 
                       "Wild Leps (Log10)",
                       "All Farm Leps",
                       "All Farm Leps (Log10)",
                       "KTC Farm Leps",
                       "KTC Farm Leps (Log10)",
                       "HSD Farm Leps",
                       "HSD Farm Leps (Log10)"))
# cut out the log values for this plot 
predict_data_long = predict_data_long %>% 
    filter(measure %in% c("Wild Leps",
                          "All Farm Leps",
                          "KTC Farm Leps",
                          "HSD Farm Leps"))
predict_data_long_wild = predict_data_long %>% 
    filter(measure == "Wild Leps")

# plots ========================================================================
summary(log_wild_to_log_all_farms)
summary(log_wild_to_log_ktc_farms)
summary(log_wild_to_log_hsd_farms)

test_mod1 = lm(all_lep ~ all_farms, data = predict_data)
test_mod2 = lm(all_lep ~ hsd_leps, data = predict_data)
test_mod3 = lm(all_lep ~ ktc_leps, data = predict_data)
summary(test_mod1)
summary(test_mod2)
summary(test_mod3)


orig_scale1 = ggplot(data = predict_data,
       aes(x = all_farms, y = all_lep)) +
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on All Farms",
       y = "Lice on Wild Fish",
       title = "All Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 2000000,
           y = 11.0,
           label = paste("R^2 ==", 0.40), 
           size = 7,
           parse = TRUE)
orig_scale2 = ggplot(data = predict_data,
                     aes(x = ktc_leps, y = all_lep)) +
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on KTC Farms",
       y = "Lice on Wild Fish",
       title = "KTC Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 2000000,
           y = 11.0,
           label = paste("R^2 ==", 0.33), 
           size = 7,
           parse = TRUE)
orig_scale3 = ggplot(data = predict_data,
                     aes(x = hsd_leps, y = all_lep)) +
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on HSD Farms",
       y = "Lice on Wild Fish",
       title = "HSD Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 2000000,
           y = 11.0,
           label = paste("R^2 ==", 0.6524), 
           size = 7,
           parse = TRUE)

all_scale = orig_scale1 + orig_scale2 + orig_scale3
ggsave(filename = here::here(
  "./figs/orig-scalewild-to-farm-models-comparison.png"),
       plot = all_scale,
       width = 15,
       height = 5,
       dpi = 600)





p1 = ggplot(data = predict_data, 
            aes(x = log_all_farms, y = log_all_lep, fill = log_all_lep)) + 
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on All Farms (Log10)",
       y = "Lice on Wild Fish (Log10)",
       title = "All Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 5.2,
           y = 1.0,
           label = paste("R^2 ==", 0.71), 
           size = 7,
           parse = TRUE)

p2 = ggplot(data = predict_data, 
            aes(x = log_ktc_leps, y = log_all_lep, fill = log_all_lep)) + 
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on Knight-Tribune Farms (Log10)",
       y = "Lice on Wild Fish (Log10)",
       title = "KTC Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 5.1,
           y = 1.0,
           label = paste("R^2 ==", 0.7), 
           size = 7,
           parse = TRUE)

p3 = ggplot(data = predict_data, 
            aes(x = log_hsd_leps, y = log_all_lep, fill = log_all_lep)) + 
  geom_point(
    shape = 21, 
    colour = "black",
    size = 4
  ) + 
  stat_smooth(method = stats::lm, formula = y ~ x) + 
  scale_fill_gradientn(
    colours = rev(PNWColors::pnw_palette("Sunset2",
                                         type = "continuous"))
  ) + 
  labs(x = "Lice on HSD Farms (Log10)",
       y = "Lice on Wild Fish (Log10)",
       title = "HSD Farms"
  ) +
  theme_mod_comp() + 
  annotate(geom = "text",
           x = 4.0,
           y = 0.8,
           label = paste("R^2 ==", 0.36), 
           size = 7,
           parse = TRUE)

p_all = p1 + p2 + p3
ggsave(filename = here::here("./figs/wild-to-farm-models-comparison.png"),
        plot = p_all,
        width = 15,
        height = 5,
        dpi = 600)

# raw_data_timeline = ggplot(data = predict_data_long) +
#     geom_point(aes(x = year, y = value, fill = value), 
#                 shape = 21, 
#                 colour = "black",
#                 size = 3.5) + 
#     stat_smooth(aes(x = year, y = value),
#                     colour = "black") +
#     facet_wrap(~measure,
#         nrow = 2, ncol = 2, scales = "fixed") +
#     labs(x = "Year", y = "Number of Lice per Fish") +
#     scale_fill_gradientn(
#         colours = rev(PNWColors::pnw_palette("Sunset2",
#                                         type = "continuous"))) +
#     theme_raw_comp()
# ggsave(filename = here::here("./figs/wild-to-farm-raw-data-comparison.png"),
#         plot = raw_data_timeline,
#         width = 20,
#         height = 8,
#         dpi = 600)

ggplot(data = scfs_regress) + 
  geom_point(aes(x = date, y = all_lep))
