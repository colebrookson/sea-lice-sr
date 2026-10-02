#' DESCRIPTION: we need a year by stage table of l. salmonis proportions to
#' do the overall imputation on
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

library(ggplot2)

fish_df <- readr::read_csv(
  cfg$path$clean_fish
)

POOLED <- TRUE # see empirical_prop() docs

# get empirical proportions from speciated years -------------------------------
emp_props <- fish_df |>
  dplyr::group_by(year) |>
  dplyr::summarize(
    p_mot = dplyr::if_else(
      dplyr::first(mot_speciated),
      empirical_prop(lep_mot_obs, sp_mot, POOLED),
      NA_real_
    ),
    p_cope = dplyr::if_else(
      dplyr::first(cope_speciated),
      empirical_prop(lep_cope_obs, sp_cope, POOLED),
      NA_real_
    ),
    .groups = "drop"
  )

# logistic regression fit to speciated years -----------------------------------
#' we just want the response to be the speciated lice only and the predictor is
#' the total for that stage, including the unid's
mot_fit_df <- fish_df |>
  dplyr::filter(mot_speciated, all_mot > 0)
cope_fit_df <- fish_df |>
  dplyr::filter(cope_speciated, all_cope > 0)

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

# predict onto the unspeciated years -------------------------------------------
#' we want to average the indiv predictions on the fish that carry lice of
#' that stage, i.e. the same population the model was fit on, an dthe only fish
#' that the proportion can be applied to.

# predict onto the 2001 for the mots
p_mot_2001 <- mean_pred(
  mot_model,
  fish_df |> dplyr::filter(year == 2001, all_mot > 0)
)

# copes for '02 to '04
p_cope_0204 <- fish_df |>
  dplyr::filter(year %in% c(2002:2004), all_cope > 0) |>
  dplyr::group_by(year) |>
  dplyr::group_modify(\(d, k) {
    tibble::tibble(
      p_cope_model = mean(stats::predict(
        cope_model,
        newdata = d,
        type = "response"
      ))
    )
  }) |>
  dplyr::ungroup()

d01 <- fish_df |> dplyr::filter(year == 2001)

# old predictor this is just to double check!
m_old <- glm(
  cbind(lep_mot_obs, cal_mot_obs) ~ all_mot,
  binomial,
  data = mot_fit_df
)
mean(predict(m_old, d01, type = "response"))

range(mot_fit_df$all_lice) # fitted support
quantile(d01$all_lice[d01$all_mot > 0], c(0.5, 0.9, 0.99, 1))
mean(d01$all_lice[d01$all_mot > 0] > max(mot_fit_df$all_lice)) #

# put proportions together -----------------------------------------------------
#' now, chalimus is the average of the lep proportions for copepodites and for
#' mots, but note that in 2001 chalimus in that year uses only mots as the
#' proportion since copes were counted as chalimus
props <- emp_props |>
  dplyr::left_join(p_cope_0204, by = "year") |>
  dplyr::mutate(
    p_mot = dplyr::if_else(year == 2001, p_mot_2001, p_mot),
    p_cope = dplyr::coalesce(p_cope, p_cope_model),
    p_chal = dplyr::if_else(
      year == 2001,
      p_mot,
      (p_mot + p_cope) / 2
    )
  ) |>
  dplyr::select(year, p_mot, p_cope, p_chal)

# each year needs motile and chalimus proportion
stopifnot(
  !any(is.na(props$p_mot)),
  !any(is.na(props$p_chal)),
  !any(is.na(props$p_cope[props$year != 2001]))
)
readr::write_csv(
  props,
  cfg$path$lep_props
)

# make some supplementary figures ----------------------------------------------
#' get CIs on the link scale and back-transform them (fit +/- 1.96 * se)

# make / save the figures (for the SI) -----------------------------------------
mot_extrap <- mean(
  fish_df$all_lice[fish_df$year == 2001 & fish_df$all_mot > 0] >
    max(mot_fit_df$all_lice)
)

save_fig(
  plot_stage(
    mot_fit_df,
    mot_model,
    "all_lice",
    "lep_mot_obs",
    "sp_mot",
    "Total lice on fish",
    "red2"
  ),
  name = "motile-model-predictions",
  dir = cfg$path$count_reg_figs,
  width = 8,
  height = 6,
  caption = paste(
    "Proportion of motile sea lice that were L. salmonis as a",
    "function of the total number of lice (all stages, all species) on an",
    "individual fish. Points are the observed proportion among speciated",
    "motiles for each fish carrying at least one motile louse in a year in",
    "which motiles were speciated (2002-present), not to worry,",
    " jittered horizontally",
    "only. Line is a binomial GLM with a logit link, Lep motiles as",
    "successes and Caligus motiles as failures, so each fish is weighted by",
    "the number of motile lice it carries. This model supplies the L. salmonis
     proportion for 2001, the only year in which motiles were counted but",
    "never speciated; the mean predicted proportion across 2001 fish carrying",
    "motiles is",
    sprintf("%.3f.", p_mot_2001),
    sprintf(
      "Fitted support spans %g-%g total lice;",
      min(mot_fit_df$all_lice),
      max(mot_fit_df$all_lice)
    ),
    sprintf("%.1f%% of 2001 fish fall outside it.", 100 * mot_extrap)
  )
)

save_fig(
  plot_stage(
    cope_fit_df,
    cope_model,
    "all_lice",
    "lep_cope_obs",
    "sp_cope",
    "Total lice on fish",
    "blue2"
  ),
  name = "cope-model-predictions",
  dir = cfg$path$count_reg_figs,
  width = 8,
  height = 6,
  caption = paste(
    "Fig. S1. Proportion of copepodite sea lice that were L. salmonis as a",
    "function of the total number of lice (all stages, all species) on an",
    "individual fish. Points are the observed proportion among speciated",
    "copepodites for each fish carrying at least one copepodite in a year",
    "in which copepodites were speciated (2005-present). Line is a binomial",
    "GLM with a logit link, Lep copepodites as successes and Caligus",
    "copepodites as failures. Ribbon is a 95% interval built on the link",
    "scale and back-transformed. This model supplies the L. salmonis",
    "proportion for 2002-2004, the years in which copepodites were counted",
    "but never speciated; the mean predicted proportions are",
    paste(
      sprintf("%d: %.3f", p_cope_0204$year, p_cope_0204$p_cope_model),
      collapse = ", "
    ),
    ". Both panels share an x-axis, so S1 and S2 are directly comparable."
  )
)
