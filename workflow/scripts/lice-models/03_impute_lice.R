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
#' but i'm not gonna do that atm its too much work so for now: 
readr::write_csv(
    replicates %>% dplyr::filter(rep == 1) %>% dplyr::select(-rep),
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)

# so now, just a quick check ---------------------------------------------------
# need to make sure all the numbers add up !!




#' FIX THIS!!!!!!!!



chk <- replicates %>%
    dplyr::filter(rep == 1) %>%
    dplyr::mutate(
        mot_ok = dplyr::near(lep_mot + cal_mot_tot, all_mot),
        cope_ok = dplyr::near(lep_cope + cal_cope_tot, all_cope),
        chal_ok = dplyr::near(lep_chal + cal_chal, all_chal)
    ) %>%
    # a stage passes if it balances OR both sides are NA (stage not counted)
    dplyr::mutate(
        mot_ok = mot_ok  | (is.na(lep_mot + cal_mot_tot) & is.na(all_mot)),
        cope_ok = cope_ok | (is.na(lep_cope + cal_cope_tot) & is.na(all_cope)),
        chal_ok = chal_ok | (is.na(lep_chal + cal_chal) & is.na(all_chal))
    ) %>%
    dplyr::filter(!(mot_ok & cope_ok & chal_ok))

if (nrow(chk) > 0) {
    stop(nrow(chk), " fish fail per-stage conservation.")
}





r1 <- replicates %>% dplyr::filter(rep == 1) %>%
    dplyr::mutate(
        recovered = rowSums(cbind(
            lep_mot, cal_mot_tot, lep_cope, cal_cope_tot, lep_chal, cal_chal
        ), na.rm = TRUE),
        diff = recovered - all_lice
    )

# where do the mismatches live?
r1 %>% dplyr::filter(abs(diff) > 1e-8) %>% dplyr::count(year)

# and pull one to inspect the components
r1 %>% dplyr::filter(abs(diff) > 1e-8) %>%
    dplyr::select(year, all_lice, recovered, diff,
                  lep_mot, cal_mot_tot, all_mot,
                  lep_cope, cal_cope_tot, all_cope,
                  lep_chal, cal_chal, all_chal) %>%
    dplyr::slice_head(n = 5) %>%
    as.data.frame()

r1 %>%
    dplyr::filter(year == 2001, all_mot > 0, is.na(lep_mot)) %>%
    dplyr::select(year, all_mot, unid_mot_n, lep_mot_obs, cal_mot_obs,
                  p_mot, lep_mot_imp) %>%
    dplyr::slice_head(n = 5) %>%
    as.data.frame()
