#' DESCRIPTION: beta-regression variant of Scenario 3,  EXCEPT the
#' 2001 unidentified-motile proportion comes from a beta regression on the
#' yearly means rather than the forced-through-origin nls. Unlike the
#' nls, beta regression does not force the intercept through 0 and yields an
#' uncertainty band, so we ALSO run the analysis at the lower end of the 95% CI
#' for the 2001 estimate. We therefore get point + lower.
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
library(ggplot2)
library(magrittr)

fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)

POOLED <- TRUE 

# yearly motile summaries (same X / Y as Scenario 3) ---------------------------
yearly <- fish_df %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(
        x_mot = mean(all_mot, na.rm = TRUE),
        y_lep = empirical_prop(lep_mot_obs, sp_mot, POOLED),
        speciated = dplyr::first(mot_speciated),
        .groups = "drop"
    )

fit_df <- yearly %>% dplyr::filter(speciated, !is.na(y_lep)) # 2002-2021
x_2001 <- yearly$x_mot[yearly$year == 2001]

# beta regression on yearly means ----------------------------------------------
#' beta needs Y in the open (0, 1). Smithson & Verkuilen squeeze pulls exact
#' 0s/1s just inside the boundary: y' = (y*(n-1) + 0.5) / n 
n_obs  <- nrow(fit_df)
fit_df <- fit_df %>%
    dplyr::mutate(y_sv = (y_lep * (n_obs - 1) + 0.5) / n_obs)

beta_fit <- betareg::betareg(y_sv ~ x_mot, data = fit_df)

# point prediction at 2001 -----------------------------------------------------
p_mot_2001_point <- as.numeric(
    stats::predict(beta_fit, newdata = tibble::tibble(x_mot = x_2001),
                   type = "response")
)

# lower 95% CI on the 2001 mean via simulation vcov ----------------------------
#' betareg has no closed-form interval on the fitted mean, so draw the mean
#' coefficients from their sampling distribution, push each through the logit
#' link at X_2001, and take the 2.5th percentile on the response scale.
set.seed(20260120)
cf_mean <- stats::coef(beta_fit, model = "mean")
V_mean <- stats::vcov(beta_fit, model = "mean")
sims <- MASS::mvrnorm(2e4, mu = cf_mean, Sigma = V_mean)
eta2001 <- sims %*% c(1, x_2001) # logit link 
p_mot_2001_lower <- as.numeric(stats::quantile(plogis(eta2001), 0.025))

# build both proportion tables -------------------------------------------------
make_props <- function(p_2001) {
    yearly %>%
        dplyr::transmute(
            year,
            p_mot  = dplyr::if_else(year == 2001, p_2001, y_lep),
            p_cope = 1,
            p_chal = 1
        )
}
props_point <- make_props(p_mot_2001_point)
props_lower <- make_props(p_mot_2001_lower)

readr::write_csv(props_point,
    here::here("./data/scfs-data/clean/lep-proportions-scenario4-point.csv"))
readr::write_csv(props_lower,
    here::here("./data/scfs-data/clean/lep-proportions-scenario4-lower.csv"))

# impute both variants ---------------------------------------------------------
M <- 100

run_variant <- function(props, tag) {
    reps <- purrr::map_dfr(
        seq_len(M),
        \(m) impute_once(fish_df, props, seed = 20260120 + m, by = "year") %>%
            dplyr::mutate(rep = m)
    )
    readr::write_csv(reps, here::here(sprintf(
        "./data/scfs-data/clean/lice-counts-imputed-replicates-scenario4-%s.csv", tag
    )))

    r1 <- reps %>% dplyr::filter(rep == 1)
    bad <- r1 %>% dplyr::filter(!(
        (dplyr::near(lep_mot + cal_mot_tot, all_mot) | 
            (is.na(lep_mot + cal_mot_tot) & is.na(all_mot)))  &
        (dplyr::near(lep_cope + cal_cope_tot, all_cope) | 
            (is.na(lep_cope + cal_cope_tot) & is.na(all_cope))) &
        (dplyr::near(lep_chal + cal_chal, all_chal) | 
            (is.na(lep_chal + cal_chal) & is.na(all_chal)))
    ))
    if (nrow(bad) > 0) {
        stop(nrow(bad), " fish fail per-stage louse conservation (", tag, ").")
    }

    readr::write_csv(
        r1 %>% dplyr::mutate(lice_for_model = all_leps) %>% dplyr::select(-rep),
        here::here(sprintf(
            "./data/scfs-data/clean/lice-counts-for-regression-scenario4-%s.csv", 
            tag
        ))
    )
    invisible(NULL)
}

run_variant(props_point, "point")
run_variant(props_lower, "lower")

# figure of the beta fit + envelope --------------------------------------------
grid <- tibble::tibble(
    x_mot = seq(0, max(yearly$x_mot, na.rm = TRUE), length.out = 400)
)
Xg <- cbind(1, grid$x_mot)
eta_g <- Xg %*% t(sims) # 400 x 20000 on the link scale
p_g <- plogis(eta_g)
grid$fit   <- as.numeric(stats::predict(
    beta_fit, newdata = grid, type = "response"
))
grid$lower <- apply(p_g, 1, stats::quantile, 0.025)
grid$upper <- apply(p_g, 1, stats::quantile, 0.975)

pred_pts <- tibble::tibble(
    x_mot = x_2001,
    y_lep = c(p_mot_2001_point, p_mot_2001_lower),
    kind  = c("point", "lower 95%")
)

p_s4 <- ggplot() +
    geom_ribbon(
        data = grid, aes(x_mot, ymin = lower, ymax = upper),
        fill = "grey80", alpha = 0.7
    ) +
    geom_line(data = grid, aes(x_mot, fit), linewidth = 1.1) +
    geom_point(
        data = fit_df, aes(x_mot, y_lep),
        shape = 21, colour = "black", fill = "orange", size = 3
    ) +
    geom_point(
        data = pred_pts, aes(x_mot, y_lep, fill = kind),
        shape = 21, colour = "black", size = 4
    ) +
    scale_fill_manual(values = c("point" = "purple", 
     "lower 95%" = "steelblue")) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
        x = "Mean motiles per fish (all species)",
        y = "Proportion L. salmonis (motiles)", fill = "2001 estimate"
    ) +
    theme_better()

save_fig(
    p_s4,
    name = "scenario4-betareg-motile-fit",
    dir  = here::here("./figs/count-regressions"),
    width = 8, height = 6,
    caption = paste(
        "Fig. S4 (Scenario 4). Beta regression (logit link) relating the yearly",
        "pooled proportion of speciated motiles that were L. salmonis to the",
        "yearly mean number of motiles per fish (all species). Unlike the",
        "Scenario 3 nls, the intercept is not forced through the origin and the",
        "fit carries an uncertainty band (grey, 95% simulated from the",
        "mean-model coefficient covariance). Orange points are the speciated",
        "fit years (2002-2021); the purple point is the predicted 2001 value",
        "and the blue point its lower 95% bound, which seeds the sensitivity",
        "run.",
        sprintf("2001 proportion = %.3f (point), %.3f (lower).",
                p_mot_2001_point, p_mot_2001_lower)
    )
)
