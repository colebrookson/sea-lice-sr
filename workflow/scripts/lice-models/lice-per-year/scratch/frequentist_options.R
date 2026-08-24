#' DESCRIPTION: frequentist random-effect structure comparison on the
#' three-stage data, then, the motile-only reference fit
#'
#' The two frameworks are fit to different response vectors — three-stage runs
#' on ~127k rows, motile-only on 56000 — so they are NOT comparable by AIC or
#' by a likelihood ratio test. The motile-only fit is compared on predictions

#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(ggplot2)

fig_dir <- here::here("./figs/freq-model-comparison")
tab_dir <- here::here("./outputs/freq-model-comparison")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

fit_cache <- here::here("./data/scfs-data/clean/frequentist-fits.qs2")
refit <- !file.exists(fit_cache)

# data -------------------------------------------------------------------------
#' 04_data_format.R must be run twice, once with motile_only_bool TRUE and once
#' FALSE
dat_stages <- qs2::qs_read(
  paste0(
    here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-all-stages-for-regression.qs2"
  )
)
dat_motile <- qs2::qs_read(
  paste0(
    here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.qs2"
  )
)

#' the year-by-stage interaction as a single factor. 2001 has no copepodid
#' rows, so the grid is ragged
dat_stages <- dat_stages |>
  dplyr::mutate(ys_f = droplevels(interaction(year_f, stage, drop = TRUE)))

# fits -------------------------------------------------------------------------
#' three-stage fits carry use as the reference level of `stage`, so in
#' every additive fit the year_f coefficients ARE the motile cell means

fits_stages <- list(
  `ly:shared wk:shared` = glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (1 | ly_f),
    family = glmmTMB::nbinom2,
    data = dat_stages
  ),
  `ly:unstr wk:shared` = glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (0 + stage | ly_f),
    family = glmmTMB::nbinom2,
    data = dat_stages
  ),
  `ly:diag wk:shared` = glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2,
    data = dat_stages
  ),
  `ly:diag wk:diag` = glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (0 + stage || week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2,
    data = dat_stages
  ),
  `ly:diag wk:unstr` = glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (0 + stage | week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2,
    data = dat_stages
  )
)

#' the interaction fit shares its RE structure with `ly:diag wk:diag`, which
#' is therefore the additive comparator
fit_ys_int <- glmmTMB::glmmTMB(
  count ~ 0 + ys_f + (0 + stage || week_f) + (0 + stage || ly_f),
  family = glmmTMB::nbinom2,
  data = dat_stages
)

#' this should be the one we want -- scalar week and location-year REs.
#' nbinom2 is variance mu*(1 + mu/phi), which is
#' exactly neg_binomial_2_log's parameterization, so sigma() maps onto Stan's
#' phi in case we need to compare
fit_motile <- glmmTMB::glmmTMB(
  count ~ 0 + year_f + (1 | week_f) + (1 | ly_f),
  family = glmmTMB::nbinom2,
  data = dat_motile
)

qs2::qs_save(
  list(
    fits_stages = fits_stages,
    fit_ys_int = fit_ys_int,
    fit_motile = fit_motile
  ),
  fit_cache
)


# convergence ------------------------------------------------------------------
#' check they all have a positive-definite Hessian
converged <- vapply(
  c(
    fits_stages,
    list(`ys interaction` = fit_ys_int, `motile-only` = fit_motile)
  ),
  \(m) isTRUE(m$sdr$pdHess),
  logical(1)
)
if (!all(converged)) {
  stop(
    "non-convergent fits: ",
    paste(names(converged)[!converged], collapse = ", ")
  )
}

# AIC over the three-stage set -------------------------------------------------
aic_stages <- do.call(
  AIC,
  c(unname(fits_stages), list(fit_ys_int))
)
aic_stages <- tibble::tibble(
  model = c(names(fits_stages), "ys interaction"),
  df = aic_stages$df,
  aic = aic_stages$AIC
) |>
  dplyr::arrange(aic) |>
  dplyr::mutate(delta_aic = aic - min(aic))

readr::write_csv(aic_stages, file.path(tab_dir, "aic-three-stage.csv"))

#' does letting year vary freely by stage beat constraining it to a common
#' shape plus a stage offset?
lrt_ys <- anova(fits_stages[["ly:diag wk:diag"]], fit_ys_int)
readr::write_csv(
  tibble::as_tibble(lrt_ys, rownames = "model"),
  file.path(tab_dir, "lrt-year-by-stage.csv")
)

# is motile-only good? ---------------------------------------------------------
#' Can't use AIC so i'm going to do a recovery check instead
#' if the additive constraint is bad, the interaction estimates move
#' away, but IFF motile-only is a good reduction rather than a different
#' model, it lands on the interaction estimates
year_coefs <- function(fit, pattern = "^year_f", strip = "^year_f") {
  fe <- glmmTMB::fixef(fit)$cond
  keep <- grep(pattern, names(fe))
  v <- sqrt(diag(vcov(fit)$cond))[keep]
  tibble::tibble(
    year = as.integer(gsub(strip, "", gsub("\\.mot$", "", names(fe)[keep]))),
    est = unname(fe[keep]),
    se = unname(v)
  )
}

motile_recovery <- dplyr::bind_rows(
  year_coefs(fits_stages[["ly:diag wk:diag"]]) |>
    dplyr::mutate(source = "additive"),
  year_coefs(fit_ys_int, pattern = "\\.mot$", strip = "^ys_f") |>
    dplyr::mutate(source = "interaction"),
  year_coefs(fit_motile) |>
    dplyr::mutate(source = "motile-only")
)

recovery_wide <- motile_recovery |>
  dplyr::select(year, source, est) |>
  tidyr::pivot_wider(names_from = source, values_from = est) |>
  dplyr::mutate(
    add_vs_motile = additive - `motile-only`,
    int_vs_motile = interaction - `motile-only`
  ) |>
  dplyr::arrange(year)

readr::write_csv(recovery_wide, file.path(tab_dir, "motile-year-recovery.csv"))

recovery_summary <- tibble::tibble(
  comparison = c("additive vs motile-only", "interaction vs motile-only"),
  max_abs_diff = c(
    max(abs(recovery_wide$add_vs_motile)),
    max(abs(recovery_wide$int_vs_motile))
  ),
  rmse = c(
    sqrt(mean(recovery_wide$add_vs_motile^2)),
    sqrt(mean(recovery_wide$int_vs_motile^2))
  )
)
readr::write_csv(
  recovery_summary,
  file.path(tab_dir, "motile-recovery-summary.csv")
)

p_recovery <- ggplot(
  motile_recovery,
  aes(year, est, colour = source, shape = source)
) +
  geom_errorbar(
    aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
    width = 0,
    linewidth = 0.4,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(size = 1.8, position = position_dodge(width = 0.6)) +
  labs(
    x = "Year",
    y = "Motile year effect (log scale)",
    colour = NULL,
    shape = NULL
  ) +
  theme_better()

save_fig(
  p_recovery,
  name = "motile-year-recovery",
  dir = fig_dir,
  width = 10,
  height = 5,
  caption = paste(
    "Motile-stage year effects on the log scale from three fits: the",
    "three-stage additive model (year shared across stages plus a stage",
    "offset), the three-stage model with the year-by-stage interaction, and",
    "the standalone motile-only model. Bars are 95% intervals from",
    "fixed-effect uncertainty. The three-stage fits use ~127k rows and the",
    "motile-only fit 56,236, so these are not comparable by AIC or likelihood",
    "ratio; the comparison is one of point recovery only. Numeric",
    "differences are in motile-recovery-summary.csv."
  )
)

# predicted motile lice per fish, two ways -------------------------------------
#' The reported quantity is mean motile lice per fish per year with the random
#' effects removed. There are two ways to remove them
#'
#' Closed form, exp(eta + sigma^2/2), integrates over the full Gaussian the REs
#' are assumed to be drawn from. AFAICT, this means "what would the mean be over
#'  all possible weeks and locations", including combos never sampled that year
#'
#' Observed-cell averaging takes conditional modes for the week and
#' l-y cells that WERE sampled in that year and averages the  means over the
#' rows as they occur. This is more like asking  "what was the mean over the
#' sampling design that happened".
#'
#' total motile-stage RE variance.
motile_re_var <- function(fit) {
  vc <- glmmTMB::VarCorr(fit)$cond
  pick <- function(mat) {
    if (is.null(mat)) {
      return(0)
    }
    m <- as.matrix(mat)
    d <- diag(m)
    if (length(d) == 1L) {
      return(unname(d[1]))
    }
    hit <- grep("mot", rownames(m))
    if (length(hit) == 0L) {
      stop("no motile entry in VarCorr block")
    }
    unname(d[hit[1]])
  }
  pick(vc$week_f) + pick(vc$ly_f)
}

w_closed <- function(fit) {
  yc <- year_coefs(fit)
  s2 <- motile_re_var(fit)
  tibble::tibble(
    year = yc$year,
    w = exp(yc$est + s2 / 2),
    lo = exp(yc$est - 1.96 * yc$se + s2 / 2),
    hi = exp(yc$est + 1.96 * yc$se + s2 / 2),
    method = "closed form"
  )
}

#' predict() with the default re.form includes the conditional modes, so this
#' works identically across every RE structure
w_observed <- function(fit, dat) {
  dat$mu <- predict(fit, type = "response")
  if ("stage" %in% names(dat)) {
    dat <- dat[dat$stage == "mot", ]
  }
  dat |>
    dplyr::group_by(year) |>
    dplyr::summarise(w = mean(mu), .groups = "drop") |>
    dplyr::mutate(lo = NA_real_, hi = NA_real_, method = "observed cells")
}

# the across-framework figure --------------------------------------------------
#' Andrew's published structure, the E17 candidate, and the motile-only model,
#' with each under both W methods
framework <- list(
  `3-stage, both shared` = list(
    fit = fits_stages[["ly:shared wk:shared"]],
    dat = dat_stages
  ),
  `3-stage, both diagonal` = list(
    fit = fits_stages[["ly:diag wk:diag"]],
    dat = dat_stages
  ),
  `motile-only` = list(fit = fit_motile, dat = dat_motile)
)

w_all <- purrr::imap_dfr(framework, \(x, label) {
  dplyr::bind_rows(w_closed(x$fit), w_observed(x$fit, x$dat)) |>
    dplyr::mutate(model = label)
})

readr::write_csv(w_all, file.path(tab_dir, "predicted-motile-by-model.csv"))

p_framework <- ggplot(w_all, aes(year, w, colour = model)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.6) +
  facet_wrap(~method, ncol = 1, scales = "free_y") +
  labs(
    x = "Year",
    y = "Predicted motile lice per fish",
    colour = NULL
  ) +
  theme_better()

save_fig(
  p_framework,
  name = "predicted-motile-across-frameworks",
  dir = fig_dir,
  width = 10,
  height = 8,
  caption = paste(
    "Predicted mean motile lice per fish per year under three model",
    "structures and two methods of removing the random effects. Top panel",
    "integrates over the assumed RE Gaussian via exp(eta + sigma^2/2); bottom",
    "panel averages fitted means over the location-week cells actually",
    "sampled in each year. The two panels are on free y scales and should be",
    "read for shape and for between-model spread, not compared by height.",
    "The three-stage fits use ~127k rows and the motile-only fit 56,236, so",
    "no AIC or likelihood ratio comparison between frameworks is possible;",
    "these are point predictions only. Intervals are omitted because the",
    "observed-cell method has no closed-form fixed-effect interval."
  )
)

#' how much does the choice of method move the answer, per model
w_ratio <- w_all |>
  dplyr::select(year, model, method, w) |>
  tidyr::pivot_wider(names_from = method, values_from = w) |>
  dplyr::mutate(ratio = `observed cells` / `closed form`)

readr::write_csv(w_ratio, file.path(tab_dir, "w-method-ratio.csv"))

p_ratio <- ggplot(w_ratio, aes(year, ratio, colour = model)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.6) +
  labs(
    x = "Year",
    y = "Observed-cell W / closed-form W",
    colour = NULL
  ) +
  theme_better()

save_fig(
  p_ratio,
  name = "w-method-ratio",
  dir = fig_dir,
  width = 10,
  height = 5,
  caption = paste(
    "Ratio of observed-cell to closed-form predicted motile lice per fish, by",
    "year and model structure. Values away from one are years in which the",
    "realized sampling design and the assumed random-effect Gaussian imply",
    "different means. The two methods differ both because of design weighting",
    "and because conditional modes are shrunk toward zero while the",
    "sigma^2/2 correction is not, so the ratio does not isolate either",
    "cause on its own."
  )
)

# the five-structure figure, retained ------------------------------------------
#' Kept as the within-three-stage RE comparison behind E16/E17. Closed form
#' only, all three stages, five structures.
re_var_by_stage <- function(fit, stages) {
  vc <- glmmTMB::VarCorr(fit)$cond
  grp <- function(mat, stage) {
    if (is.null(mat)) {
      return(0)
    }
    m <- as.matrix(mat)
    d <- diag(m)
    if (length(d) == 1L) {
      return(unname(d[1]))
    }
    key <- paste0("stage", stage)
    if (key %in% rownames(m)) unname(d[key]) else unname(d[1])
  }
  vapply(
    stages,
    \(s) grp(vc$week_f, s) + grp(vc$ly_f, s),
    numeric(1)
  )
}

stage_lv <- levels(dat_stages$stage)
year_lv <- levels(dat_stages$year_f)

predict_one <- function(fit, label) {
  grid <- expand.grid(
    year_f = factor(year_lv, levels = year_lv),
    stage = factor(stage_lv, levels = stage_lv),
    week_f = NA,
    ly_f = NA
  )
  pr <- predict(fit, newdata = grid, se.fit = TRUE, re.form = NA, type = "link")
  rv <- re_var_by_stage(fit, stage_lv)
  half <- rv[as.character(grid$stage)] / 2
  tibble::tibble(
    model = label,
    year = as.integer(as.character(grid$year_f)),
    stage = grid$stage,
    mean = exp(pr$fit + half),
    lo = exp(pr$fit - 1.96 * pr$se.fit + half),
    hi = exp(pr$fit + 1.96 * pr$se.fit + half)
  )
}

pred_all <- purrr::imap_dfr(fits_stages, \(f, label) predict_one(f, label))

p_structures <- ggplot(pred_all, aes(year, mean, colour = model)) +
  geom_errorbar(
    aes(ymin = lo, ymax = hi),
    width = 0,
    linewidth = 0.5,
    position = position_dodge(width = 0.6)
  ) +
  geom_point(size = 1.6, position = position_dodge(width = 0.6)) +
  facet_wrap(~stage, scales = "free_y", ncol = 1) +
  labs(
    x = "Year",
    y = "Mean predicted lice per fish",
    colour = "RE structure"
  ) +
  theme_better()

save_fig(
  p_structures,
  name = "predicted-lice-per-fish-by-model",
  dir = fig_dir,
  width = 9,
  height = 10,
  caption = paste(
    "Mean predicted lice per fish per year for five random-effect structures",
    "on the three-stage data, faceted by louse stage, with the random effects",
    "integrated out via exp(eta + sigma^2/2). Bars are 95% intervals from",
    "fixed-effect uncertainty only; the sigma^2/2 correction is applied at the",
    "point estimate of the variance components and its uncertainty is not",
    "propagated. Motile is the reported stage. AIC ordering for these five",
    "structures is in aic-three-stage.csv."
  )
)


wk <- glmmTMB::ranef(fit_motile)$cond$week_f[[1]]
qqnorm(wk)
qqline(wk)
c(n = length(wk), sd_modes = sd(wk), sigma_fitted = 2.129)
