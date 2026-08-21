#' DESCRIPTION: These are some easy helper functions for the species-level
#' imputation that we have to do on the wild lice. Essentially, the raw data
#' can't really tell whether a blank is a zero or missing (see SCFS data,
#' "We also do not distinguish zeroes from missing data for the louse
#' information"). So we do NOT infer it from the data.
#' AUTHOR: Cole Brookson

#' lice_protocol
#'
#' @description taken from the SCFS metadata, we look to describe stages as
#' counted or speciated in a given year
#' @param years the years of the data
#'
#' @return a tibble showing the stage/counted information for given years
lice_protocol <- function(years) {
  tibble::tibble(year = years) |>
    dplyr::mutate(
      cope_counted = year >= 2002,
      cope_speciated = year >= 2005,
      mot_counted = TRUE,
      mot_speciated = year >= 2002,
      chal_counted = TRUE,
      chal_speciated = FALSE
    )
}

# ---- column groups -----------------------------------------------------------
COPE_COLS <- c("lep_cope", "cal_cope", "unid_cope")
CHAL_COLS <- c("chala", "chalb", "chal_unid")
MOT_COLS <- c(
  "lep_pamale",
  "lep_pafemale",
  "lep_male",
  "lep_nongravid",
  "lep_gravid",
  "cal_mot",
  "cal_gravid",
  "unid_adult",
  "unid_pa"
)

#' fill_counted_stages
#'
#' @description Within a stage that was actually counted, if it's blank the
#' counter examined the fish, found none of that category, so it's a zero. e.g.
#' in 2002-2004, lep_cope = 0, because no copepodites were identified as leps
#' cos no one looked. These lice are all in the `unid_cope` column. So, for a
#' stage that wasn't counted, each column stays NA, and every derived total
#' stays NA.
#' @param df the dataframe that we're working with
fill_counted_stages <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(COPE_COLS),
        \(x) dplyr::if_else(cope_counted & is.na(x), 0, x)
      ),
      dplyr::across(
        dplyr::any_of(MOT_COLS),
        \(x) dplyr::if_else(mot_counted & is.na(x), 0, x)
      ),
      dplyr::across(
        dplyr::any_of(CHAL_COLS),
        \(x) dplyr::if_else(chal_counted & is.na(x), 0, x)
      )
    )
}

#' draw_leps
#'
#' @description for each unidentified louse, draw Bernoulli(p) to decide if it
#' is an L. salmonis. A sum of `n` iid Bernoulli draws is just Binomial(n,p),
#' so we do rbinom(size = n, prob = p) to vectorize the bernoulli draw. To keep
#' the size NA (if the stage isn't counted), NA out. size 0 -> 0 out, no draw.
#' prob NA with size > 0 -> doesn't r87n.
#' @param size the number of draws
#' @param prob the probability needed
draw_leps <- function(size, prob) {
  out <- rep(NA_real_, length(size))

  is_zero <- !is.na(size) & size == 0
  out[is_zero] <- 0

  needs <- !is.na(size) & size > 0
  if (any(needs & is.na(prob))) {
    stop(
      "draw_leps(): ",
      sum(needs & is.na(prob)),
      " fish have unidentified lice but no L. salmonis proportion. ",
      "Check the proportion table."
    )
  }
  out[needs] <- stats::rbinom(
    sum(needs),
    size = size[needs],
    prob = prob[needs]
  )
  return(out)
}

#' empirical_prop
#'
#' @description we need a pooled proportion of: total lep / total speciated,
#' across all fish in the year. We can set pooled = FALSE to take the mean of
#' ratios, but this gives a fish carrying 1 louse the same weight as a fish
#' carrying 40 lice, so the mean-of-ratios is biased downwards.
#' @param lep
#' @param spec
#' @param pooled = TRUE
empirical_prop <- function(lep, spec, pooled = TRUE) {
  if (pooled) {
    denom <- sum(spec, na.rm = TRUE)
    if (denom == 0) {
      return(NA_real_)
    }
    sum(lep, na.rm = TRUE) / denom
  } else {
    mean(
      dplyr::if_else(
        spec > 0,
        lep / spec,
        NA_real_
      ),
      na.rm = TRUE
    )
  }
}

#' impute_once
#'
#' @description given some variables, do a single imputation via draws of the
#' leps for different stages
#'
#' @param fish_df dataframe of the relevant data
#' @param props the proportions
#' @param seed a reproducible seed
impute_once <- function(fish_df, props, seed, by = "year") {
  set.seed(seed)

  df <- dplyr::left_join(fish_df, props, by = by)
  df |>
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
        cbind(lep_mot, lep_cope, lep_chal),
        na.rm = TRUE
      ),
      week = lubridate::isoweek(lubridate::make_date(year, month, day))
    )
}

#' mean_pred
#'
#' @description We want to average the individual predictions on the fish that
#' carry lice of a given stage
#'
#' @param model the fitted model
#' @param newdata what to predict onto
#'
#' @returns the response we want
mean_pred <- function(model, newdata) {
  if (nrow(newdata) == 0) {
    return(NA_real_)
  }
  mean(stats::predict(model, newdata = newdata, type = "response"))
}


pred_ribbon <- function(model, xvar, xmax) {
  nd <- tibble::tibble(!!xvar := seq(0, xmax, length.out = 500))
  p <- stats::predict(model, newdata = nd, type = "link", se.fit = TRUE)
  inv <- model$family$linkinv

  nd |>
    dplyr::mutate(
      fit = inv(p$fit),
      lower = inv(p$fit - 1.96 * p$se.fit),
      upper = inv(p$fit + 1.96 * p$se.fit)
    )
}

plot_stage <- function(fit_df, model, xvar, lep, spec, xlab, fill) {
  rib <- pred_ribbon(model, xvar, max(fit_df[[xvar]]))
  points <- fit_df |>
    dplyr::filter(.data[[spec]] > 0) |>
    dplyr::mutate(obs_prop = .data[[lep]] / .data[[spec]])

  ggplot() +
    geom_point(
      data = points,
      aes(x = .data[[xvar]], y = obs_prop),
      shape = 21,
      colour = "black",
      fill = fill,
      alpha = 0.1,
      size = 3,
      position = position_jitter(height = 0)
    ) +
    geom_ribbon(
      data = rib,
      aes(x = .data[[xvar]], ymin = lower, ymax = upper),
      fill = "grey80",
      alpha = 0.7
    ) +
    geom_line(
      data = rib,
      aes(x = .data[[xvar]], y = fit),
      linewidth = 1.2
    ) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = xlab, y = "Proportion L. salmonis") +
    theme_better()
}
