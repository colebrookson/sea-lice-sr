#' DESCRIPTION: take in the raw data with lice counts on fish and format it so
#' I can easily do regression on it
#' DATE: 20 January 2026
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)

fish_df <- readr::read_csv(
    here::here("./data/scfs-data/raw/fish-data.csv")
) %>%
    standardize_names(.) %>%
    dplyr::select(
        year, day, month, location,
        unid_cope, lep_cope, cal_cope, lep_cope, chala, chalb,
        chal_unid, lep_pamale, lep_male, lep_nongravid, lep_gravid,
        lep_pafemale, cal_mot, cal_gravid, unid_adult, unid_pa
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        # all motile leps
        lep_mot = sum(
            c(
                lep_pamale, lep_male, lep_nongravid,
                lep_gravid, lep_pafemale, unid_pa
            ),
            na.rm = TRUE
        ),
        # all lice
        all_lice = sum(
            c(
                lep_cope, chala, chalb, chal_unid, lep_pamale,
                lep_male, lep_nongravid, lep_gravid, lep_pafemale,
                cal_cope, cal_mot, cal_gravid, unid_cope, unid_adult, unid_pa
            ),
            na.rm = TRUE
        ),
        # all speciated motiles
        all_sp_mot = sum(
            c(
                lep_pamale, lep_male, lep_nongravid, lep_gravid, lep_pafemale,
                cal_mot, cal_gravid, unid_pa
            ),
            na.rm = TRUE
        ),
        # all motiles
        all_mot = sum(
            c(
                lep_pamale, lep_male, lep_nongravid, lep_gravid, lep_pafemale,
                cal_mot, cal_gravid, unid_adult, unid_pa
            ),
            na.rm = TRUE
        ),
        # all speciated copes
        all_sp_cope = sum(
            c(lep_cope, cal_cope),
            na.rm = TRUE
        ),
        # all copes
        all_cope = sum(
            c(lep_cope, cal_cope, unid_cope),
            na.rm = TRUE
        ),
        # all chalimus
        all_chal = sum(
            c(chala, chalb, chal_unid),
            na.rm = TRUE
        ),
        # proportion of motiles that are leps
        prop_lep_mot = lep_mot / all_sp_mot,
        # proportion of copepodids that are leps
        prop_lep_cope = lep_cope / all_sp_cope
    )

readr::write_csv(
    fish_df,
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)


# some plotting stuff just for fun
# add in unique identifier to make life easier later
fish_df$obs_id <- c(1:nrow(fish_df))
fish_march_april <- fish_df %>% dplyr::filter(month %in% c(3,4))
ggplot(data = fish_df) + 
    geom_point(aes(x = year, y = all_lice, fill = month), position = 
    position_jitter(), colour = "black", shape = 21, size = 0.5) + 
    scale_x_continuous(
        breaks = unique(fish_df$year), labels = unique(fish_df$year)) + 
    scale_fill_manual(values = MoMAColors::moma.)
    theme_better()

focal_months <- c("3", "4", "5", "6")

fish_df <- fish_df |>
    dplyr::mutate(
        is_zero = all_lice == 0,
        month_focal = factor(
            dplyr::if_else(
                as.character(month) %in% focal_months,
                as.character(month), NA_character_
            ),
            levels = focal_months
        )
    )

panel_levels <- c("Month 3", "Month 4", "Month 5", "Month 6", "All months")

plot_df <- fish_df |>
    tidyr::expand_grid(panel = factor(panel_levels, levels = panel_levels)) |>
    dplyr::mutate(
        is_zero = all_lice == 0,
        m = as.character(month),
        highlight = factor(
            dplyr::case_when(
                panel == "Month 3" & m == "3" ~ "3",
                panel == "Month 4" & m == "4" ~ "4",
                panel == "Month 5" & m == "5" ~ "5",
                panel == "Month 6" & m == "6" ~ "6",
                panel == "All months" & m %in% focal_months ~ m,
                .default = NA_character_
            ),
            levels = focal_months
        )
    )

pal <- MoMAColors::moma.colors("Klein", type = "discrete")
focal_cols <- c("3" = pal[1], "4" = pal[3], "5" = pal[2], "6" = pal[5])

ggplot2::ggplot(plot_df, ggplot2::aes(x = year, y = all_lice)) +
    ggplot2::geom_point(
        data = \(d) dplyr::filter(d, is_zero),
        position = ggplot2::position_jitter(width = 0.3, height = 0, seed = 1),
        shape = 16, colour = "grey75", size = 0.3, alpha = 0.2
    ) +
    ggplot2::geom_point(
        data = \(d) dplyr::filter(d, !is_zero, is.na(highlight)),
        position = ggplot2::position_jitter(width = 0.3, height = 0, seed = 1),
        shape = 16, colour = "grey65", size = 0.6, alpha = 0.3
    ) +
    ggplot2::geom_point(
        data = \(d) dplyr::filter(d, !is_zero, !is.na(highlight)),
        ggplot2::aes(fill = highlight),
        position = ggplot2::position_jitter(width = 0.3, height = 0, seed = 1),
        shape = 21, colour = "grey20", stroke = 0.15, size = 0.9, alpha = 0.85
    ) +
    ggplot2::scale_fill_manual(values = focal_cols, name = "Month") +
    ggplot2::scale_x_continuous(
        breaks = seq(min(fish_df$year), max(fish_df$year), by = 5)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(panel), ncol = 2) +
    ggplot2::guides(fill = ggplot2::guide_legend(
        override.aes = list(size = 3, alpha = 1))) +
    theme_better()


fish_df |>
    dplyr::mutate(
        m = as.character(month),
        month_grp = factor(
            dplyr::if_else(m %in% focal_months, m, "Other"),
            levels = c("Other", focal_months)
        )
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = year, fill = month_grp)) +
    ggplot2::geom_bar() +
    ggplot2::scale_fill_manual(
        values = c(focal_cols, Other = "grey80"),
        breaks = c(focal_months, "Other"),
        name = "Month"
    ) +
    ggplot2::scale_x_continuous(
        breaks = seq(min(fish_df$year), max(fish_df$year), by = 5)
    ) +
    ggplot2::labs(y = "Observations") +
    theme_better()


lice_vars   <- c("all_lice", "lep_mot", "all_sp_mot")
metric_labs <- c(all_lice   = "All lice",
                 lep_mot    = "Motile Lepeophtheirus",
                 all_sp_mot = "All motile lice")
metric_cols <- MoMAColors::moma.colors("Klein", type = "discrete")[c(1, 3, 6)]
names(metric_cols) <- lice_vars

lice_month_plot <- function(data, intensity = FALSE) {
    long <- data |>
        tidyr::pivot_longer(dplyr::all_of(lice_vars),
                            names_to = "metric", values_to = "count") |>
        dplyr::filter(!is.na(count))

    if (intensity) long <- dplyr::filter(long, count > 0)

    long <- dplyr::mutate(long, metric = factor(metric, levels = lice_vars))

    ggplot2::ggplot(long, ggplot2::aes(x = factor(month), y = count)) +
        ggplot2::geom_jitter(
            width = 0.25, height = 0,
            colour = "grey75", size = 0.3, alpha = 0.15) +
        ggplot2::stat_summary(
            ggplot2::aes(group = metric, colour = metric),
            fun = mean, geom = "line", linewidth = 0.4) +
        ggplot2::stat_summary(
            ggplot2::aes(colour = metric),
            fun.data = ggplot2::mean_cl_boot,
            geom = "errorbar", width = 0.2, linewidth = 0.4) +
        ggplot2::stat_summary(
            ggplot2::aes(colour = metric),
            fun = mean, geom = "point", size = 1.8) +
        ggplot2::scale_colour_manual(values = metric_cols, guide = "none") +
        ggplot2::facet_wrap(ggplot2::vars(metric), ncol = 1, scales = "free_y",
            labeller = ggplot2::as_labeller(metric_labs)) +
        ggplot2::labs(
            x = "Month",
            y = if (intensity) "Lice per infested fish" else "Lice per fish") +
        theme_better()
}

p_abundance <- lice_month_plot(fish_df, intensity = FALSE)
p_intensity <- lice_month_plot(fish_df, intensity = TRUE)

save_fig(p_abundance, "lice_by_month_abundance", height = 8,
    caption = paste(
        "Monthly mean louse counts across ALL sampled fish, zeros included,",
        "so each series is mean abundance: the expected load on a randomly",
        "sampled fish, which folds prevalence and per-fish burden into one",
        "number. Coloured points are means, error bars are 95% bootstrap CIs",
        "on the mean (mean_cl_boot, 1000 resamples); faint grey points are",
        "raw per-fish counts, jittered on month only so the count axis is",
        "exact. Panels use FREE y scales, so vertical distances are not",
        "comparable across metrics. Broughton data through 2025."))

save_fig(p_intensity, "lice_by_month_intensity", height = 8,
    caption = paste(
        "Monthly mean louse counts across INFESTED fish only, zeros dropped",
        "per metric, so each series is mean intensity: the typical burden on",
        "fish that actually carry lice, isolating per-fish load from",
        "prevalence. A month can rise here while abundance falls if fewer",
        "fish are infested but those infested carry more, so read this",
        "alongside the abundance figure rather than instead of it. Means",
        "with 95% bootstrap CIs; faint grey points are raw counts, month",
        "jitter only. FREE y scales. Broughton data through 2025."))