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
# add in unique identifier to make life easier later
fish_df$obs_id <- c(1:nrow(fish_df))
readr::write_csv(
    fish_df,
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)
