#' DESCRIPTION: this file re-formats the data to then be used by different
#' scripts that run different pieces of the modeling pipeline/process
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)


collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)

# get rid of the weeks we don't want here
collated_df_long <- collated_df %>%
    dplyr::filter(week %notin% c(9, 28, 33)) %>%
    tidyr::pivot_longer(
        cols = c(lep_mot, lep_cope, lep_chal),
        names_to = "stage",
        names_prefix = "lep_", # so values are mot/cope/chal
        values_to = "count"
    ) %>%
    # drop the 2001 cope NA rows (only NA cell, per the audit)
    dplyr::filter(!is.na(count)) %>%
    dplyr::select(obs_id, count, stage, year, week, location) %>%
    dplyr::mutate(
        # motile first so stage_idx == 1 is the reference 
        stage = factor(stage, levels = c("mot", "cope", "chal")),
        # location-year from observed combinations only 
        ly = factor(paste(location, year, sep = "_")),
        # factor the grouping vars AFTER 
        year_f = droplevels(factor(year)),
        week_f = droplevels(factor(week)),
        ly_f = droplevels(ly)
    )

# integer index vectors for NIMBLE
collated_df_long <- collated_df_long %>%
    dplyr::mutate(
        year_idx = as.integer(year_f),
        week_idx = as.integer(week_f),
        stage_idx = as.integer(stage),
        ly_idx = as.integer(ly_f)
    )

# level maps — idx -> label, so posteriors map back
year_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$year_f)),
    year = levels(collated_df_long$year_f)
)
week_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$week_f)),
    week = levels(collated_df_long$week_f)
)
stage_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$stage)),
    stage = levels(collated_df_long$stage)
)
ly_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$ly_f)),
    ly = levels(collated_df_long$ly_f)
)
# a couple quick data checks ---------------------------------------------------

# check this has all been done right with the following: 
# collated_df_long %>% dplyr::count(stage) # no NAs
# collated_df_long %>% dplyr::count(year, stage) %>% # only 2001 is empty at cope
#     tidyr::pivot_wider(names_from = stage, values_from = n, values_fill = 0)
# collated_df_long %>% dplyr::count(ly_f) %>% dplyr::arrange(n) # how many loc-yr?
# # how many distinct locations, and are any suspiciously near-duplicates?
# collated_df_long %>% dplyr::distinct(location) %>% dplyr::arrange(location)

# # and the site x year grid — is 75 a clean function of sites x years-observed?
# collated_df_long %>% dplyr::count(location, year) %>%
#     tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
#     print(width = Inf)

readr::write_csv(
    collated_df_long, 
    paste0(here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.csv")
)