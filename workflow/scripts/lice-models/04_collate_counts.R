#' DESCRIPTION: Once the motile and copepodite counts are imputed, we'll do the
#' chalimus stage imputation, and then collate all the counts
#' AUTHOR: Cole Brookson
#' DATE: 21 January 2026

source(here::here("./R/functions/theme_better.R"))
source(here::here("./R/functions/global.R"))

library(magrittr)
library(ggplot2)

imputed_copes <- readr::read_csv(
    here::here("./data/scfs-data/clean/copes-imputed.csv")
)
imputed_mots <- readr::read_csv(
    here::here("./data/scfs-data/clean/mots-imputed.csv")
)
fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)

# join together the datasets with the imputed values ---------------------------

#' fish_df will keep most of it's columns, but `lep_mot`, `cal_mot`,  and
#' `avg_prop_mots` will come from the imputed_mots and `lep_cope`, `cal_cope`,
#' and `avg_prop_copes` will come from imputed_copes
collated_df <- fish_df %>%
    dplyr::select(-c(lep_mot, cal_mot, lep_cope, cal_cope)) %>%
    dplyr::left_join(
        .,
        y = imputed_copes %>%
            dplyr::select(obs_id, lep_cope, cal_cope, avg_prop_copes),
        by = "obs_id"
    ) %>%
    dplyr::left_join(
        .,
        y = imputed_mots %>%
            dplyr::select(obs_id, lep_mot, cal_mot, avg_prop_mots),
        by = "obs_id"
    )
# impute the chalimus stage lice -----------------------------------------------
to_impute_rows <- which(!(is.na(collated_df$all_chal) |
    collated_df$all_chal == 0))
collated_df$lep_chal <- 0
collated_df$cal_chal <- 0
for (i in to_impute_rows) {
    if (collated_df$year[i] == 2001) {
        # assign probability for 2005
        probs <- c(
            collated_df[[i, "avg_prop_mots"]], # prob of drawing lep (1)
            (1 - collated_df[[i, "avg_prop_mots"]])
        )
    } else {
        # assign otherwise
        probs <- c(
            mean(c(
                collated_df[[i, "avg_prop_mots"]],
                collated_df[[i, "avg_prop_copes"]]
            )),
            1 - mean(c(
                collated_df[[i, "avg_prop_mots"]],
                collated_df[[i, "avg_prop_copes"]]
            ))
        )
    }
    lice <- sample(
        x = c(1, 0),
        size = collated_df[[i, "all_chal"]], # size is the number to sample
        replace = TRUE,
        prob = probs
    )
    # add leps to the leps column (lep_mot)
    collated_df$lep_chal[i] <- collated_df$lep_chal[i] +
        # how many 1s are in the lice vector
        length(which(lice == 1))
    # add cals to the cal column (cal_mot)
    collated_df$cal_chal[i] <- collated_df$cal_chal[i] +
        # how many 0s are in the lice vector
        length(which(lice == 0))
}

# sum all the lep lice ---------------------------------------------------------
collated_df <- collated_df %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        all_leps = my_sum(lep_mot, lep_cope, lep_chal)
    )

# extract week of year ---------------------------------------------------------
collated_df <- collated_df %>%
    dplyr::mutate(
        week = lubridate::isoweek(lubridate::make_date(year, month, day))
    )
# write final data -------------------------------------------------------------
readr::write_csv(
    collated_df,
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)
