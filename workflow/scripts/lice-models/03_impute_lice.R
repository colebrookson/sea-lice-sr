#' DESCRIPTIOn: Do the bernoulli imputation against the year by stage proportion
#' table from the previous file. All three stages are handled 
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
library(magrittr)
 
fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)
props <- readr::read_csv(
    here::here("./data/scfs-data/clean/lep-proportions.csv")
)

# realization of imputations ---------------------------------------------------
#' so the props will be what gets changed if the scenarios change, so when I 
#' write scenario 2 it shouldn't be much difference 
impute_once <- function(fish_df, props, seed, by = "year") {
    set.seed(seed)

    df <- dplyr::left_join(fish_df, props, by = by)
    df %>% 
        dplyr::mutate(
            # motiles are done by re-allocating the unid_adult (and unid_pa
            # in the 2001 case)
            lep_mot_imp = draw_leps(unid_mot_n, p_mot),
            lep_mot = lep_mot_obs + lep_mot_imp, 
            cal_mot_tot = cal_mot_obs + (unid_mot_n - lep_mot_imp), 

            # now the copes just have the re-allocation of unid_cope
            lep_cope_imp = draw_leps(unid_cope_n, p_cope), 
            lep_cope = lep_cope_obs + lep_cope_imp, 
            cal_cope_tot = cal_cope_obs + (unid_cope_n - lep_cope_imp),

            # now finally the fkn chalimus 
            lep_chal = draw_leps(unid_chal_n, p_chal),
            cal_chal = unid_chal_n - lep_chal,

            # now get the total leps 
            all_leps = rowSums(
                cbind(lep_mot, lep_cope, lep_chal), na.rm = TRUE
            ), 
            week = lubridate::isoweek(lubridate::make_date(year, month, day))
        )
}

# now run M replicates ---------------------------------------------------------
#' since the imputation is stochastic, we run it once and it carries the 
#' realization forward. Multiple M gets us some uncertainty!
M <- 100

replicates <- purrr::map_dfr(
    seq_len(M),
    \(m) impute_once(fish_df, props, seed = 20260120 + m) %>% 
    dplyr::mutate(rep = m)
)

readr::write_csv(
    replicates, 
    here::here("./data/scfs-data/clean/lice-counts-imputed-replicates.csv")
)

# get a single point-estimate value --------------------------------------------
#' I think the best way to do this is to fit to all M and pool the answer... 