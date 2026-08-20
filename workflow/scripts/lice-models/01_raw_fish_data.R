#' DESCRIPTION: take in the raw data with lice counts on fish and format it so
#' I can easily do regression on it
#' DATE: 20 January 2026
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
library(magrittr)

if (exists("snakemake")) {
  fish_data_path <- snakemake@inputs[["fish_data"]]
  standardized_fish_path <- snakemake@outputs[["standardized_fish"]]
} else {
  fish_data_path <- here::here("./data/scfs-data/raw/fish-data.csv")
  standardized_fish_path <- here::here(
    "./data/scfs-data/clean/standardized-fish-data.csv"
  )
}

raw <- readr::read_csv(
  fish_data_path
) |>
  standardize_names() |>
  dplyr::select(
    year,
    day,
    month,
    location,
    dplyr::all_of(
      c(COPE_COLS, CHAL_COLS, MOT_COLS)
    )
  ) |>
  # obs_id has to exist now, because i use it as a join key later
  dplyr::mutate(obs_id = dplyr::row_number())

# attach the protocol and zero-fill --------------------------------------------
fish_df <- raw |>
  dplyr::left_join(lice_protocol(unique(raw$year)), by = "year") |>
  fill_counted_stages()

#' quick sanity echck -- 2001 should have no speciated motiles
#' `lep_nongravid` has zero NAs in 2001 which is somewhat odd? If there are
#' non-zeros then we check now
spec_2001 <- fish_df |>
  dplyr::filter(year == 2001) |>
  dplyr::summarise(dplyr::across(
    dplyr::all_of(c(
      "lep_pamale",
      "lep_pafemale",
      "lep_male",
      "lep_nongravid",
      "lep_gravid",
      "cal_mot",
      "cal_gravid"
    )),
    \(x) sum(x, na.rm = TRUE)
  ))
if (any(spec_2001 > 0)) {
  warning(
    "2001 has nonzero values in speciated motile columns ",
    paste(utils::capture.output(print(spec_2001)), collapse = "\n")
  )
}

# ok sweet g2g!

# resolve unid_pa --------------------------------------------------------------
#' in the SCFS metadata, we see that pre-adult leps were sexed in 2002-2003 and
#' 2008-onwards, but NOT in 2004-2007. Motile caligus were never separated
#' to pre-adult vs adult, so an unsexed pre-adult in 2002+ has already been
#' speciated AS lep just with no sex
#'
#' 2001 should have unid_pa as pre-adult of unkown species
#' 2002+ should have unid_pa as lep pre-adult
fish_df <- fish_df |>
  dplyr::mutate(
    lep_pa_unsexed = dplyr::if_else(year >= 2002, unid_pa, 0),
    unid_pa_sp = dplyr::if_else(year == 2001, unid_pa, 0)
  )

# stage totals! ----------------------------------------------------------------
fish_df <- fish_df |>
  dplyr::mutate(
    lep_cope_obs = lep_cope,
    cal_cope_obs = cal_cope,
    sp_cope = lep_cope + cal_cope,
    unid_cope_n = unid_cope,
    all_cope = sp_cope + unid_cope,

    # now the motiles
    lep_mot_obs = lep_pamale +
      lep_pafemale +
      lep_male +
      lep_nongravid +
      lep_gravid +
      lep_pa_unsexed,
    cal_mot_obs = cal_mot + cal_gravid,
    sp_mot = lep_mot_obs + cal_mot_obs,
    unid_mot_n = unid_adult + unid_pa_sp,
    all_mot = sp_mot + unid_mot_n,

    # now the chalimus, all unid
    unid_chal_n = chala + chalb + chal_unid,
    all_chal = unid_chal_n,

    # all lice, any stage/species (in 2001 the copes were classified as
    # chalimus, so na.rm = TRUE here)
    all_lice = rowSums(
      cbind(all_cope, all_mot, all_chal),
      na.rm = TRUE
    )
  )

readr::write_csv(
  fish_df,
  standardized_fish_path
)
