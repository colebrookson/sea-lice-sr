#' DESCRIPTION: The second scenario which is the same as the first, minus the
#' model-filled cells (2001 mots and 2002-2004 copes) use per-fish
#' predicted proportion from the scenario 1 glms instead of a year-averaged
#' scalar. Chals are indiviaulized on whatever componnet has a per-fish value
#' AUTHOR: Cole Brookson

# load things ! ----------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("./config/config.yaml"))

fish_df <- readr::read_csv(cfg$path$clean_fish)

POOLED <- TRUE # this is for empirical_prop()

# refit the scenario 1 ---------------------------------------------------------
#' this is literally only so this runs without having to source elsewhere

mot_fit_df <- fish_df |> dplyr::filter(mot_speciated, all_mot > 0)
cope_fit_df <- fish_df |> dplyr::filter(cope_speciated, all_cope > 0)

mot_model <- stats::glm(
  cbind(lep_mot_obs, cal_mot_obs) ~ all_lice,
  family = stats::binomial(link = "logit"),
  data = mot_fit_df
)
cope_model <- stats::glm(
  cbind(lep_cope_obs, cal_cope_obs) ~ all_lice,
  family = stats::binomial(link = "logit"),
  data = cope_fit_df
)

# get the empirical year proportions (speciated year values) -------------------
emp_props <- fish_df |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    p_mot_emp = dplyr::if_else(
      dplyr::first(mot_speciated),
      empirical_prop(lep_mot_obs, sp_mot, POOLED),
      NA_real_
    ),
    p_cope_emp = dplyr::if_else(
      dplyr::first(cope_speciated),
      empirical_prop(lep_cope_obs, sp_cope, POOLED),
      NA_real_
    ),
    .groups = "drop"
  )

# now get the per-fish proportion table ----------------------------------------
props_s2 <- fish_df |>
  dplyr::select(obs_id, year, all_lice) |>
  dplyr::left_join(emp_props, by = "year")

#' we want the per-fish predictions from the scenario 1 glms, but evaluate it
#' at each fish's lice load, which each fish has
props_s2$p_mot_fish <- stats::predict(
  mot_model,
  newdata = props_s2,
  type = "response"
)
props_s2$p_cope_fish <- stats::predict(
  cope_model,
  newdata = props_s2,
  type = "response"
)

props_s2 <- props_s2 |>
  dplyr::mutate(
    # individualise ONLY the model-filled cells
    # everything speciated keeps the Scenario 1 empirical year value
    p_mot = dplyr::if_else(year == 2001, p_mot_fish, p_mot_emp),
    p_cope = dplyr::case_when(
      year %in% 2002:2004 ~ p_cope_fish,
      year >= 2005 ~ p_cope_emp,
      TRUE ~ NA_real_ # 2001 copes were counted as chalimus
    ),
    # chalimus = mean of the two proportions, individualised
    # on whichever component actually has a per-fish value
    p_chal = dplyr::case_when(
      year == 2001 ~ p_mot, # motile-only
      year %in% 2002:2004 ~ (p_mot_emp + p_cope_fish) / 2, # cope
      TRUE ~ (p_mot_emp + p_cope_emp) / 2 # 2005+: year scalar
    )
  ) |>
  dplyr::select(obs_id, p_mot, p_cope, p_chal)


# to check, every fish that will draw has a non-NA proportion ------------------
#' look for a bad join!
need_chk <- fish_df |>
  dplyr::select(obs_id, unid_mot_n, unid_cope_n, unid_chal_n) |>
  dplyr::left_join(props_s2, by = "obs_id") |>
  dplyr::filter(
    (unid_mot_n > 0 & is.na(p_mot)) |
      (unid_cope_n > 0 & is.na(p_cope)) |
      (unid_chal_n > 0 & is.na(p_chal))
  )
if (nrow(need_chk) > 0) {
  stop(nrow(need_chk), " fish have unidentified lice but missing proportion.")
}

# run M replicates -------------------------------------------------------------
#' same seed base as 03 so draws are paired with Scenario 1
replicates <- purrr::map_dfr(
  seq_len(cfg$run$num_M),
  \(m) {
    impute_once(fish_df, props_s2, seed = 20260120 + m, by = "obs_id") |>
      dplyr::mutate(rep = m)
  }
)
readr::write_csv(replicates, here::here(cfg$path$scenarios$scen_2_reps))


# double check again -----------------------------------------------------------
r1 <- replicates |> dplyr::filter(rep == 1)
bad <- r1 |>
  dplyr::filter(
    !((dplyr::near(lep_mot + cal_mot_tot, all_mot) |
      (is.na(lep_mot + cal_mot_tot) & is.na(all_mot))) &
      (dplyr::near(lep_cope + cal_cope_tot, all_cope) |
        (is.na(lep_cope + cal_cope_tot) & is.na(all_cope))) &
      (dplyr::near(lep_chal + cal_chal, all_chal) |
        (is.na(lep_chal + cal_chal) & is.na(all_chal))))
  )
if (nrow(bad) > 0) {
  stop(nrow(bad), " fish fail per-stage louse conservation.")
}

# take one realization for now
readr::write_csv(
  r1 |>
    dplyr::mutate(lice_for_model = all_leps) |> # standard response col
    dplyr::select(-rep),
  here::here(cfg$path$scenarios$scen_2)
)
