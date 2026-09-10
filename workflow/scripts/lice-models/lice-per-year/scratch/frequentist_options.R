#' DESCRIPTION: frequentist tests
#'
#' This fits the full 2 x 5 grid — {additive, interaction} fixed effects
#' crossed with five RE structures — so the RE question can be answered under
#' the correct fixed effects, and so we can see whether the best RE structure
#' is even the same under both
#'
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(ggplot2)

fig_dir <- here::here("./figs/freq-model-comparison")
tab_dir <- here::here("./outputs/freq-model-comparison")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tab_dir, recursive = TRUE, showWarnings = FALSE)

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

#' year-by-stage as a single factor. 2001 has no copepodid rows, so the grid is
#' ragged and this has 74 levels, not 75. droplevels is what stops glmmTMB
#' generating an empty cell it cannot estimate.
dat_stages <- dat_stages |>
  dplyr::mutate(ys_f = droplevels(interaction(year_f, stage, drop = TRUE)))

stopifnot(nlevels(dat_stages$ys_f) == 74)

# the 2 x 5 grid ---------------------------------------------------------------
#' RE terms are written once and pasted onto both fixed-effect structures, so
#' the two halves of the grid differ only in the fixed part.
fe_terms <- list(
  additive = "0 + year_f + stage",
  interaction = "0 + ys_f"
)

re_terms <- list(
  `ly:shared wk:shared` = "(1 | week_f) + (1 | ly_f)",
  `ly:unstr wk:shared` = "(1 | week_f) + (0 + stage | ly_f)",
  `ly:diag wk:shared` = "(1 | week_f) + (0 + stage || ly_f)",
  `ly:diag wk:diag` = "(0 + stage || week_f) + (0 + stage || ly_f)",
  `ly:diag wk:unstr` = "(0 + stage | week_f) + (0 + stage || ly_f)"
)

#' `interaction / ly:shared wk:shared` is the Bateman replica.
grid_spec <- tidyr::expand_grid(
  fe = names(fe_terms),
  re = names(re_terms)
) |>
  dplyr::mutate(
    label = paste(fe, re, sep = " / "),
    formula = paste0(
      "count ~ ",
      unlist(fe_terms[fe]),
      " + ",
      unlist(re_terms[re])
    )
  )

if (refit) {
  message("fitting ", nrow(grid_spec), " models plus the motile reference")
  t0 <- Sys.time()

  fits <- lapply(seq_len(nrow(grid_spec)), function(i) {
    message("  ", grid_spec$label[i])
    glmmTMB::glmmTMB(
      stats::as.formula(grid_spec$formula[i]),
      family = glmmTMB::nbinom2,
      data = dat_stages
    )
  })
  names(fits) <- grid_spec$label

  #' nbinom2 is variance mu*(1 + mu/phi), exactly neg_binomial_2_log's
  #' parameterization, so sigma() maps onto Stan's phi directly
  fit_motile <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + (1 | week_f) + (1 | ly_f),
    family = glmmTMB::nbinom2,
    data = dat_motile
  )

  elapsed <- difftime(Sys.time(), t0, units = "mins")
  message("total fit time: ", round(as.numeric(elapsed), 1), " min")

  qs2::qs_save(
    list(fits = fits, fit_motile = fit_motile, elapsed_min = elapsed),
    fit_cache
  )
} else {
  cached <- qs2::qs_read(fit_cache)
  fits <- cached$fits
  fit_motile <- cached$fit_motile
}

# convergence ------------------------------------------------------------------
#' glmmTMB returns coefficients and an AIC for a fit that never reached a
#' positive-definite Hessian. The unstructured 3x3 blocks are the likely
#' failures and the interaction fits are the heaviest, so this has to pass
#' before the AIC table means anything.
converged <- vapply(
  c(fits, list(`motile-only` = fit_motile)),
  \(m) isTRUE(m$sdr$pdHess),
  logical(1)
)
if (!all(converged)) {
  stop(
    "non-convergent fits: ",
    paste(names(converged)[!converged], collapse = ", ")
  )
}

# AIC over the whole grid ------------------------------------------------------
aic_raw <- do.call(AIC, unname(fits))

aic_grid <- grid_spec |>
  dplyr::select(fe, re, label) |>
  dplyr::mutate(
    df = aic_raw$df,
    aic = aic_raw$AIC,
    delta_aic = aic - min(aic),
    bateman = fe == "interaction" & re == "ly:shared wk:shared"
  ) |>
  dplyr::arrange(aic)

readr::write_csv(aic_grid, file.path(tab_dir, "aic-grid.csv"))

#' does the RE ordering hold under both fixed-effect structures? if the two
#' columns rank differently, the RE choice is not separable from the FE choice
#' and E16/E17 cannot be carried over as-is.
aic_by_fe <- aic_grid |>
  dplyr::group_by(fe) |>
  dplyr::mutate(rank_within_fe = rank(aic)) |>
  dplyr::ungroup() |>
  dplyr::select(re, fe, aic, rank_within_fe) |>
  tidyr::pivot_wider(
    names_from = fe,
    values_from = c(aic, rank_within_fe)
  ) |>
  dplyr::arrange(aic_interaction)

readr::write_csv(aic_by_fe, file.path(tab_dir, "aic-re-ranking-by-fe.csv"))

#' additive vs interaction at the best RE structure, rather than at one picked
#' before we knew which that was. Nested, same data, so the LRT is valid.
best_re <- aic_grid$re[1]
lrt_fe <- anova(
  fits[[paste("additive", best_re, sep = " / ")]],
  fits[[paste("interaction", best_re, sep = " / ")]]
)
readr::write_csv(
  tibble::as_tibble(lrt_fe, rownames = "model"),
  file.path(tab_dir, "lrt-additive-vs-interaction.csv")
)

# motile year effects across structures ----------------------------------------
#' works for both parameterizations: under interaction the motile cells are the
#' `.mot`-suffixed ys_f coefficients; under additive they are the year_f
#' coefficients, since motile is the reference level of stage.
motile_year_coefs <- function(fit) {
  fe <- glmmTMB::fixef(fit)$cond
  if (any(grepl("^ys_f", names(fe)))) {
    keep <- grep("\\.mot$", names(fe))
    yr <- as.integer(sub("^ys_f", "", sub("\\.mot$", "", names(fe)[keep])))
  } else {
    keep <- grep("^year_f", names(fe))
    yr <- as.integer(sub("^year_f", "", names(fe)[keep]))
  }
  se <- sqrt(diag(vcov(fit)$cond))[keep]
  tibble::tibble(year = yr, est = unname(fe[keep]), se = unname(se))
}

compare_set <- list(
  `Bateman (int, both shared)` = fits[["interaction / ly:shared wk:shared"]],
  `best from grid` = fits[[aic_grid$label[1]]],
  `motile-only` = fit_motile
)

motile_recovery <- purrr::imap_dfr(
  compare_set,
  \(f, label) motile_year_coefs(f) |> dplyr::mutate(source = label)
)

recovery_wide <- motile_recovery |>
  dplyr::select(year, source, est) |>
  tidyr::pivot_wider(names_from = source, values_from = est) |>
  dplyr::arrange(year) |>
  dplyr::mutate(
    bateman_vs_motile = `Bateman (int, both shared)` - `motile-only`,
    best_vs_motile = `best from grid` - `motile-only`
  )

readr::write_csv(recovery_wide, file.path(tab_dir, "motile-year-recovery.csv"))

recovery_summary <- tibble::tibble(
  comparison = c("Bateman vs motile-only", "best-from-grid vs motile-only"),
  max_abs_diff = c(
    max(abs(recovery_wide$bateman_vs_motile)),
    max(abs(recovery_wide$best_vs_motile))
  ),
  rmse = c(
    sqrt(mean(recovery_wide$bateman_vs_motile^2)),
    sqrt(mean(recovery_wide$best_vs_motile^2))
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
    "Motile-stage year effects on the log scale from three fits: the Bateman",
    "et al. (2016) structure (year-by-stage fixed effects, scalar random",
    "effects shared across stages), the structure ranked first by AIC in the",
    "2x5 grid, and the standalone motile-only model. Bars are 95% intervals",
    "from fixed-effect uncertainty. The three-stage fits and the motile-only",
    "fit use different response vectors, so the comparison is one of point",
    "recovery only, not model selection. Numeric differences are in",
    "motile-recovery-summary.csv."
  )
)

# predicted motile lice per fish, two ways -------------------------------------
#' Closed form, exp(eta + sigma^2/2), integrates over the full Gaussian the REs
#' are assumed to be drawn from: the mean over all weeks and location-years,
#' including combinations never sampled in that year.
#'
#' Observed-cell averaging takes the conditional modes for the cells that were
#' sampled and averages the fitted means over rows as they occur: the mean over
#' the sampling design that happened.
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
  yc <- motile_year_coefs(fit)
  s2 <- motile_re_var(fit)
  tibble::tibble(
    year = yc$year,
    w = exp(yc$est + s2 / 2),
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
    dplyr::mutate(method = "observed cells")
}

framework <- list(
  `Bateman (int, both shared)` = list(
    fit = fits[["interaction / ly:shared wk:shared"]],
    dat = dat_stages
  ),
  `best from grid` = list(fit = fits[[aic_grid$label[1]]], dat = dat_stages),
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
  labs(x = "Year", y = "Predicted motile lice per fish", colour = NULL) +
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
    "integrates over the assumed random-effect Gaussian via",
    "exp(eta + sigma^2/2); bottom panel averages fitted means over the",
    "location-week cells actually sampled in each year. The two panels are on",
    "free y scales and should be read for shape and for between-model spread,",
    "not compared by height. The three-stage fits and the motile-only fit use",
    "different response vectors, so these are point predictions only and no",
    "likelihood comparison between them is possible. Intervals are omitted",
    "because the observed-cell method has no closed-form fixed-effect",
    "interval."
  )
)

w_ratio <- w_all |>
  tidyr::pivot_wider(names_from = method, values_from = w) |>
  dplyr::mutate(ratio = `observed cells` / `closed form`)

readr::write_csv(w_ratio, file.path(tab_dir, "w-method-ratio.csv"))

p_ratio <- ggplot(w_ratio, aes(year, ratio, colour = model)) +
  geom_hline(yintercept = 1, linetype = 2, colour = "grey50") +
  geom_line(linewidth = 0.5) +
  geom_point(size = 1.6) +
  labs(x = "Year", y = "Observed-cell W / closed-form W", colour = NULL) +
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
    "sigma^2/2 correction is not, so the ratio does not isolate either cause",
    "on its own."
  )
)

# per-stage predictions across the five RE structures --------------------------
#' Now run at the interaction fixed effects rather than the additive ones. The
#' prediction grid is built from observed year-stage combinations, so the
#' missing 2001 copepodid cell drops out rather than erroring.
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
  vapply(stages, \(s) grp(vc$week_f, s) + grp(vc$ly_f, s), numeric(1))
}

stage_lv <- levels(dat_stages$stage)

pred_grid <- dat_stages |>
  dplyr::distinct(year_f, stage, ys_f) |>
  dplyr::mutate(week_f = NA, ly_f = NA)

predict_one <- function(fit, label) {
  pr <- predict(
    fit,
    newdata = pred_grid,
    se.fit = TRUE,
    re.form = NA,
    type = "link"
  )
  half <- re_var_by_stage(fit, stage_lv)[as.character(pred_grid$stage)] / 2
  tibble::tibble(
    model = label,
    year = as.integer(as.character(pred_grid$year_f)),
    stage = pred_grid$stage,
    mean = exp(pr$fit + half),
    lo = exp(pr$fit - 1.96 * pr$se.fit + half),
    hi = exp(pr$fit + 1.96 * pr$se.fit + half)
  )
}

int_labels <- grid_spec$label[grid_spec$fe == "interaction"]
int_fits <- fits[int_labels]
names(int_fits) <- grid_spec$re[grid_spec$fe == "interaction"]

pred_all <- purrr::imap_dfr(int_fits, \(f, label) predict_one(f, label))

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
    "Mean predicted lice per fish per year for five random-effect structures,",
    "all fit with year-by-stage interaction fixed effects, faceted by louse",
    "stage, with the random effects integrated out via exp(eta + sigma^2/2).",
    "Bars are 95% intervals from fixed-effect uncertainty only; the",
    "sigma^2/2 correction is applied at the point estimate of the variance",
    "components and its uncertainty is not propagated. The 2001 copepodid",
    "cell is absent because no copepodid lice were counted that year. AIC",
    "ordering is in aic-grid.csv."
  )
)

# week-effect normality, for the closed-form W question -------------------------
#' exp(sigma^2/2) is the exact marginal mean only if the week effects are
#' normal on the log scale. With sigma_week near 2.1 the correction is roughly
#' a factor of ten and almost all of it comes from the upper tail, so whether
#' that tail exists is the whole question.
wk <- glmmTMB::ranef(fit_motile)$cond$week_f[[1]]
sigma_wk <- sqrt(as.numeric(glmmTMB::VarCorr(fit_motile)$cond$week_f))

png(
  file.path(fig_dir, "week-conditional-modes-qq.png"),
  width = 800,
  height = 800
)
qqnorm(wk, main = "Week conditional modes, motile-only fit")
qqline(wk)
dev.off()

readr::write_csv(
  tibble::tibble(
    n = length(wk),
    sd_modes = sd(wk),
    sigma_fitted = sigma_wk,
    shrinkage_ratio = sd(wk) / sigma_wk,
    shapiro_p = stats::shapiro.test(wk)$p.value
  ),
  file.path(tab_dir, "week-mode-normality.csv")
)
