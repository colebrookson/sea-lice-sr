#' DESCRIPTIOn: Do the bernoulli imputation against the year by stage proportion
#' table from the previous file. All three stages are handled
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
library(magrittr)

if (exists("snakemake")) {
  fish_df_path <- snakemake@input[["fish_df"]]
  props_path <- snakemake@input[["props"]]
  M <- snakemake@params[["M"]]
  replicates_out_path <- snakemake@output[["replicates"]]
  point_val_out_path <- snakemake@output[["point_val"]]
} else {
  fish_df_path <- here::here(
    "./data/scfs-data/clean/standardized-fish-data.csv"
  )
  props_path <- here::here("./data/scfs-data/clean/lep-proportions.csv")
  point_val_out_path <- here::here(
    "./data/scfs-data/clean/lice-counts-for-regression.csv"
  )
  replicates_out_path <- here::here(
    "./data/scfs-data/clean/lice-counts-imputed-replicates.csv"
  )
  M <- 100
}

fish_df <- readr::read_csv(fish_df_path)
props <- readr::read_csv(props_path)

# now run M replicates ---------------------------------------------------------
#' since the imputation is stochastic, we run it once and it carries the
#' realization forward. Multiple M gets us some uncertainty!

replicates <- purrr::map_dfr(
  seq_len(M),
  \(m) {
    impute_once(fish_df, props, seed = 20260120 + m) %>%
      dplyr::mutate(rep = m)
  }
)

readr::write_csv(
  replicates,
  replicates_out_path
)

# get a single point-estimate value --------------------------------------------
#' I think the best way to do this is to fit to all M and pool the answer...
#' but i'm not gonna do that atm its too much work so for now:
readr::write_csv(
  replicates %>%
    dplyr::filter(rep == 1) %>%
    dplyr::mutate(lice_for_model = all_leps) %>%
    dplyr::select(-rep),
  point_val_out_path
)

# so now, just a quick check ---------------------------------------------------
# need to make sure all the numbers add up !!

chk <- replicates %>%
  dplyr::filter(rep == 1) %>%
  dplyr::mutate(
    mot_ok = dplyr::near(lep_mot + cal_mot_tot, all_mot),
    cope_ok = dplyr::near(lep_cope + cal_cope_tot, all_cope),
    chal_ok = dplyr::near(lep_chal + cal_chal, all_chal)
  ) %>%
  # a stage passes if it balances OR both sides are NA (stage not counted)
  dplyr::mutate(
    mot_ok = mot_ok | (is.na(lep_mot + cal_mot_tot) & is.na(all_mot)),
    cope_ok = cope_ok | (is.na(lep_cope + cal_cope_tot) & is.na(all_cope)),
    chal_ok = chal_ok | (is.na(lep_chal + cal_chal) & is.na(all_chal))
  ) %>%
  dplyr::filter(!(mot_ok & cope_ok & chal_ok))

if (nrow(chk) > 0) {
  stop(nrow(chk), " fish fail per-stage conservation.")
}

if (interactive()) {
  replicates %>%
    dplyr::filter(rep == 1) %>%
    dplyr::filter(
      (!is.na(all_mot) & !dplyr::near(lep_mot + cal_mot_tot, all_mot)) |
        (!is.na(all_cope) & !dplyr::near(lep_cope + cal_cope_tot, all_cope)) |
        (!is.na(all_chal) & !dplyr::near(lep_chal + cal_chal, all_chal))
    ) %>%
    nrow()
}
