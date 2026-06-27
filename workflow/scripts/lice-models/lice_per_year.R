#' DESCRIPTION: Fit the simple regressions that provide the basis for the
#' results here
#' AUTHOR: Cole Brookson
#' DATE: 19 January 2026

source(here::here("./R/functions/theme_better.R"))
source(here::here("./R/functions/global.R"))

library(magrittr)
library(ggplot2)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)
farm_df <- readr::read_csv(
    here::here("./data/farm-data/clean/clean-farm-data.csv")
)
# Part 1 - organize lice data for this -----------------------------------------

#' NOTE FOR LEILA, ignore this part and go ahead and use your wild lice modeled
#' values here, this is just so the model will fit for you:
wild_lice_per_year <- data.frame(
    year = c(2001:2023),
    mean_wild_lice = c(
        9.0037313, 3.9539007, 0.6508876, 5.6494940, 2.8152591,
        0.6557377, 0.6539394, 0.5535181, 0.1416242, 0.6933216, 0.2657576,
        0.4585431, 0.3129630, 0.2666865, 1.5319379, 0.6037806, 0.7668203,
        0.2712294, 0.9468723, 0.3640020, 0.9231094, 1.0608016, 0.2303622
    )
)

farm_lice_per_year <- farm_df %>%
    dplyr::filter(year %in% c(2001:2023)) %>%
    dplyr::filter(month %in% c(3, 4)) %>% # changed to march, april, and may
    dplyr::group_by(year) %>%
    dplyr::summarize(
        mean_farm_lice = my_mean(lep_tot)
    ) %>%
    dplyr::mutate(
        log_farm_lice = log10(mean_farm_lice)
    )

yearly_lice_data <- cbind(
    farm_lice_per_year[, c("year", "mean_farm_lice")],
    wild_lice_per_year[, "mean_wild_lice"]
)
yearly_lice_data$group <- "All Farms with Available Data"

simple_mod <- stats::lm(
    mean_wild_lice ~ mean_farm_lice,
    data = yearly_lice_data
)
r2_val <- summary(simple_mod)$r.squared
plot_lab <- paste("R^2 == ", round(r2_val, 2))
wild_farm_reg <- ggplot(data = yearly_lice_data) +
    geom_point(
        aes(
            x = mean_farm_lice, y = mean_wild_lice
        ),
        size = 4, shape = 21, colour = "black", fill = "#42e4e4"
    ) +
    geom_smooth(aes(x = mean_farm_lice, y = mean_wild_lice),
        formula = y ~ x, method = "lm", level = 0.95
    ) +
    labs(x = "Lice on Farmed Fish (millions)", y = "Lice on Wild Fish") +
    scale_x_log10(
        breaks = c(3e+05, 1e+06, 3e+06),
        labels = c("0.3", "1.0", "3.0")
    ) +
    scale_y_log10() +
    theme_better() +
    annotate("text",
        x = 0.3e+06, y = 3, label = plot_lab, parse = TRUE,
        size = 10
    )

ggsave(
    here::here("./figs/wild-farm-regression.png"),
    wild_farm_reg,
    dpi = 300,
    height = 8, width = 11
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
