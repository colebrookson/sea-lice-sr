#' DESCRIPTION: All chalimus and all unidentified
#' copepodites are counted as L. salmonis in EVERY year, with the 2001
#' unidentified-motile proportion comes from an asymptotic nls fit
#' Y ~ 1 - exp(-c*X) (a = 1, b = 0 fixed, c estimated), fit to yearly means.
#' X = mean motiles per fish (all species); Y = pooled proportion of speciated
#' motiles that were L. salmonis. Motile proportions in speciated years
#' are the same pooled empirical values as Scenario 1. Only the motile stage is
#' stochastic; cope/chal are all-Lep (p = 1) and so draw-free in expectation

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("./config/config.yaml"))

fish_df <- readr::read_csv(cfg$path$clean_fish)

POOLED <- TRUE

# yearly motile summaries ------------------------------------------------------
#' X = mean motiles/fish (all species) over ALL fish that year and Y = pooled
#' Lep-mot proportion among speciated motiles. one point per year.
yearly <- fish_df |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    x_mot = mean(all_mot, na.rm = TRUE),
    y_lep = empirical_prop(lep_mot_obs, sp_mot, POOLED),
    speciated = dplyr::first(mot_speciated),
    .groups = "drop"
  )

fit_df <- yearly |> dplyr::filter(speciated, !is.na(y_lep)) # 2002-2021

# asymptotic fit ---------------------------------------------------------------
#' a = 1, b = 0, each point implies c = -log(1 - Y) / X
c0 <- stats::median(-log(1 - pmin(fit_df$y_lep, 0.999)) / fit_df$x_mot)
nls_fit <- stats::nls(
  y_lep ~ 1 - exp(-c * x_mot),
  data = fit_df,
  start = list(c = c0)
)

x_2001 <- yearly$x_mot[yearly$year == 2001]
p_mot_2001 <- as.numeric(1 - exp(-stats::coef(nls_fit)[["c"]] * x_2001))

if (p_mot_2001 <= 0.9) {
  warning("2001 motile proportion < 0.9.")
}

# proportion table -------------------------------------------------------------
props_s3 <- yearly |>
  dplyr::transmute(
    year,
    p_mot = dplyr::if_else(year == 2001, p_mot_2001, y_lep),
    p_cope = 1, # all unidentified copepodites -> L. salmonis, every year
    p_chal = 1 # all chalimus -> L. salmonis, every year
  )

readr::write_csv(props_s3, cfg$path$scenarios$scen_3_props)

# impute -----------------------------------------------------------------------
M <- cfg$run$num_M
replicates <- purrr::map_dfr(
  seq_len(M),
  \(m) {
    impute_once(fish_df, props_s3, seed = 20260120 + m, by = "year") |>
      dplyr::mutate(rep = m)
  }
)
readr::write_csv(
  replicates,
  here::here(cfg$path$scenario$scen_3_reps)
)

# louse conservation checkkkk --------------------------------------------------
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

readr::write_csv(
  r1 |>
    dplyr::mutate(lice_for_model = all_leps) |>
    dplyr::select(-rep),
  here::here(cfg$path$scenarios$scen_3)
)

# figure -----------------------------------------------------------------------
grid <- tibble::tibble(
  x_mot = seq(0, max(yearly$x_mot, na.rm = TRUE), length.out = 400)
)
grid$fit <- 1 - exp(-stats::coef(nls_fit)[["c"]] * grid$x_mot)

pred_pt <- tibble::tibble(x_mot = x_2001, y_lep = p_mot_2001)

p_s3 <- ggplot() +
  geom_line(data = grid, aes(x_mot, fit), linewidth = 1.1) +
  geom_point(
    data = fit_df,
    aes(x_mot, y_lep),
    shape = 21,
    colour = "black",
    fill = "orange",
    size = 3
  ) +
  geom_point(
    data = pred_pt,
    aes(x_mot, y_lep),
    shape = 21,
    colour = "black",
    fill = "purple",
    size = 4
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    x = "Mean motiles per fish (all species)",
    y = "Proportion L. salmonis (motiles)"
  ) +
  theme_better()

save_fig(
  p_s3,
  name = "scenario3-nls-motile-fit",
  dir = here::here("./figs/count-regressions"),
  width = 8,
  height = 6,
  caption = paste(
    "Scenario 3. Asymptotic nls fit Y = 1 - exp(-c*X), a = 1 and",
    "b = 0 fixed, c estimated, relating the yearly pooled proportion of",
    "speciated motiles that were L. salmonis (Y) to the yearly mean number",
    "of motiles per fish across all species (X). Orange points are the",
    "speciated years (2002-2021) the curve was fit to; the purple point is",
    "the model-predicted 2001 value, applied to 2001's unidentified",
    "motiles. All chalimus and all unidentified copepodites are counted as",
    "L. salmonis in every year.",
    sprintf("Predicted 2001 proportion = %.3f at X = %.2f.", p_mot_2001, x_2001)
  )
)
