#' DESCRIPTION: Fit the simple regressions that provide the basis for the
#' results here
#' AUTHOR: Cole Brookson
#' DATE: 19 January 2026

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here('./workflow/scripts/functions/fit_simple.R'))
library(magrittr)
library(ggplot2)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)
farm_df <- readr::read_csv(
    here::here("./data/broughton-farm-data/all-options-compiled.csv")
)
# Part 1 - organize lice data for this -----------------------------------------

farm_lice_per_year <- farm_df %>%
    dplyr::filter(year %in% c(2001:max(collated_df$year))) %>%
    dplyr::filter(month %in% c(3, 4)) %>% # changed to march, april
    # make the total louse counts for the various options
    dplyr::mutate(
        marty_lep_tot = marty_inventory * marty_mot_lep_per_fish, 
        bati_lep_tot = bati_inventory * bati_mot_lep_per_fish, 
        av_lep_tot = my_mean(marty_lep_tot, bati_lep_tot),
        best_lep_tot = best_inventory * best_mot_lep_per_fish
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        marty_mean_lep_tot = my_mean(marty_lep_tot), 
        bati_mean_lep_tot = my_mean(bati_lep_tot),
        av_mean_lep_tot = my_mean(av_lep_tot),
        best_mean_lep_tot = my_mean(best_lep_tot)
    ) %>%
    dplyr::mutate(
        log_marty_mean_lep_tot = log10(marty_mean_lep_tot),
        log_bati_mean_lep_tot = log10(bati_mean_lep_tot),
        log_av_mean_lep_tot = log10(av_mean_lep_tot),
        log_best_mean_lep_tot = log10(best_mean_lep_tot)
    )

wild_lice_per_year <- collated_df %>% 
    dplyr::filter(month %in% c(3,4,5)) %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
        mean_all_leps = my_mean(all_leps),
        mean_lep_mots = my_mean(lep_mot)
    )


yearly_lice_data <- dplyr::left_join(
    farm_lice_per_year, 
    wild_lice_per_year,
    by = "year"
)


### I AM LEAVING OFF HERE:
#' I'm a bit confused because the numbers that look to be coming out of the farm
#' lice per year are not what I would expect? they're off by quite some bit, 
#' and when i was looking at the values (i.e. those ones plotted in retrospective) 
#' I couldn't figure out what was going on 
#' ANSWER - oh wait I figured out what was going on. I am just looking at the raw
#' averaged values per year, which is not what I want. I want to use the values 
#' that come from fitting the yearly model which is WHY i started this file with 
#' that name lol 

simple_mod <- stats::lm(log10(mean_all_leps) ~ log10(marty_mean_lep_tot), data = yearly_lice_data)
summary(simple_mod)
hist(log(yearly_lice_data$mean_all_leps))



fit_all_farms_marty <- fit_extract_plot(
    df = yearly_lice_data, 
    wild_lice = "mean_all_leps", 
    farm_lice = "mean_marty_lep_tot", 
    slug = "-mean-marty-"
    )

# try with other relationships -------------------------------------------------
farm_ktc_lice_per_year <- farm_df %>%
    dplyr::filter(year %in% c(2001:2023)) %>%
    dplyr::filter(month %in% c(3, 4)) %>% # changed to just march & april
    dplyr::filter(ktc == "Knight Tribune Corridor") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        mean_farm_lice = my_mean(lep_tot)
    ) %>%
    dplyr::mutate(
        log_farm_lice = log10(mean_farm_lice)
    )
farm_df %>%
    dplyr::filter(year %in% c(2003)) %>%
    # dplyr::filter(month %in% c(3, 4)) %>% # changed to just march & april
    dplyr::filter(ktc == "Knight Tribune Corridor") %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarize(n = dplyr::n())


syearly_ktc_lice_data <- cbind(
    farm_ktc_lice_per_year[, c("year", "mean_farm_lice")],
    wild_lice_per_year[, "mean_wild_lice"]
)
yearly_ktc_lice_data$group <- "Farms in Knight-Tribune Corridor"

wild_farm_ktc_reg <- ggplot(data = yearly_ktc_lice_data) +
    geom_point(
        aes(
            x = mean_farm_lice, y = mean_wild_lice
        ),
        size = 4, shape = 21, colour = "black", fill = "#42e4e4"
    ) +
    geom_smooth(aes(x = mean_farm_lice, y = mean_wild_lice),
        formula = y ~ x, method = "lm"
    ) +
    labs(x = "Lice on Farmed Fish (millions)", y = "Lice on Wild Fish") +
    scale_x_log10(
        # breaks = c(3e+05, 1e+06, 3e+06),
        # labels = c("0.3", "1.0", "3.0")
    ) +
    theme_better()

## HSD farms only --------------------------------------------------------------
farm_hsd_lice_per_year <- farm_df %>%
    dplyr::filter(year %in% c(2001:2023)) %>%
    dplyr::filter(hump_sarg_doc == "Humphrey-Sargeaunt-Doctors Triangle") %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        mean_farm_lice = my_mean(lep_tot)
    ) %>%
    dplyr::mutate(
        log_farm_lice = log10(mean_farm_lice)
    )
yearly_hsd_lice_data <- cbind(
    farm_hsd_lice_per_year[, c("year", "mean_farm_lice")],
    wild_lice_per_year[1:23, "mean_wild_lice"]
)
yearly_hsd_lice_data$group <- "Humphrey-Sargeaunt-Doctors Triangle"

wild_farm_hsd_reg <- ggplot(data = yearly_hsd_lice_data) +
    geom_point(
        aes(
            x = mean_farm_lice, y = mean_wild_lice
        ),
        size = 4, shape = 21, colour = "black", fill = "#42e4e4"
    ) +
    geom_smooth(aes(x = mean_farm_lice, y = mean_wild_lice),
        formula = y ~ x, method = "lm"
    ) +
    labs(x = "Lice on Farmed Fish (millions)", y = "Lice on Wild Fish") +
    scale_x_log10(
        # breaks = c(3e+05, 1e+06, 3e+06),
        # labels = c("0.3", "1.0", "3.0")
    ) +
    theme_better()


# all groupings together -------------------------------------------------------
all_groups <- rbind(
    yearly_lice_data,
    yearly_ktc_lice_data,
    yearly_hsd_lice_data
)

# fit all the simple linear models
simple_all_mod <- stats::lm(
    mean_wild_lice ~ mean_farm_lice,
    data = yearly_lice_data
)
r2_all_val <- summary(simple_mod)$adj.r.squared
all_plot_lab <- paste("R^2 == ", round(r2_val, 2))


simple_ktc_mod <- stats::lm(
    mean_wild_lice ~ mean_farm_lice,
    data = yearly_ktc_lice_data
)
r2_ktc_val <- summary(simple_ktc_mod)$adj.r.squared
ktc_plot_lab <- paste("R^2 == ", round(r2_ktc_val, 2))

simple_hsd_mod <- stats::lm(
    mean_wild_lice ~ mean_farm_lice,
    data = yearly_hsd_lice_data
)
r2_hsd_val <- summary(simple_hsd_mod)$adj.r.squared
hsd_plot_lab <- paste("R^2 == ", round(r2_hsd_val, 2))

# put the various r^2 values into a df
df_text <- data.frame(
    label = c(all_plot_lab, ktc_plot_lab, hsd_plot_lab),
    group = c(
        "All Farms with Available Data",
        "Farms in Knight-Tribune Corridor",
        "Humphrey-Sargeaunt-Doctors Triangle"
    ),
    x = 0.05, # 5% from left edge
    y = 0.95 # 95% from bottom (i.e., near top)
)

all_regs <- ggplot(data = all_groups) +
    geom_point(
        aes(
            x = mean_farm_lice, y = mean_wild_lice, fill = group
        ),
        size = 4, shape = 21, colour = "black"
    ) +
    geom_smooth(
        aes(
            x = mean_farm_lice, y = mean_wild_lice, colour = group,
            fill = group
        ),
        formula = y ~ x, method = "lm", level = 0.90
    ) +
    facet_wrap(~group, nrow = 1, scales = "free_x") +
    labs(x = "Lice on Farmed Fish (millions)", y = "Lice per Wild Fish") +
    ggpp::geom_text_npc(
        data = df_text,
        aes(npcx = x, npcy = y, label = label),
        hjust = 0,
        vjust = 1,
        parse = TRUE,
        size = 8
    ) +
    scale_x_log10(
        # breaks = c(3e+05, 1e+06, 3e+06),
        # labels = c("0.3", "1.0", "3.0")
    ) +
    scale_y_log10() +
    theme_better() +
    scale_fill_manual(
        values = c("#2E6F8E", "#29AF7F", "#BDDF26")
    ) +
    scale_colour_manual(
        values = c("#2E6F8E", "#29AF7F", "#BDDF26")
    ) +
    theme(
        legend.position = "none",
        strip.text = element_text(size = 26, face = "bold")
    )
ggsave(
    here::here("./figs/farm-wild-all-groups.png"),
    all_regs,
    dpi = 300,
    width = 25,
    height = 8
)
