#' DESCRIPTION: this file is just for the fucking around with frequentist option
#' so we can get something running and do some checks before fucking with nimble
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)
library(nimble)
library(nimbleHMC)

collated_df_long <- qs2::qs_read(
    paste0(here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.qs2")
)

# comparison to frequentist ----------------------------------------------------
# same data
tmb_fit <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (1 | ly_f),
    family = glmmTMB::nbinom2,
    data = collated_df_long
)

# summary(tmb_fit)
# glmmTMB::glmmTMB::fixef(tmb_fit)$cond # year_f coefs = beta_year; stage = beta_stage
# sigma(tmb_fit) # this is glmmTMB's phi = r (nbinom2 dispersion)
# # RE sds
# print(glmmTMB::VarCorr(tmb_fit))

## tmb random slope variety just for fun ---------------------------------------
fit_slope <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (0 + stage | ly_f),
    family = glmmTMB::nbinom2,
    data = collated_df_long
)

# summary(fit_slope)
# glmmTMB::glmmTMB::fixef(fit_slope)$cond # year_f coefs = beta_year; stage = beta_stage
# sigma(fit_slope) # this is glmmTMB's phi = r (nbinom2 dispersion)
# # RE sds
# print(glmmTMB::VarCorr(fit_slope))

fit_diag <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2, 
    data = collated_df_long
)


## is the slope or normal one better -------------------------------------------
lrt <- anova(tmb_fit, fit_slope)
print(lrt)


year_ses <- function(fit, label) {
    co <- summary(fit)$coefficients$cond
    tibble::as_tibble(co, rownames = "term") %>%
        dplyr::filter(grepl("^year_f", term)) %>%
        dplyr::transmute(
            year = as.integer(sub("^year_f", "", term)),
            est = Estimate,
            se = `Std. Error`,
            model = label
        )
}

se_compare <- dplyr::bind_rows(
    year_ses(tmb_fit, "baseline"),
    year_ses(fit_slope, "stage_slope")
) %>%
    tidyr::pivot_wider(
        names_from = model,
        values_from = c(est, se)
    ) %>%
    dplyr::mutate(se_ratio = se_stage_slope / se_baseline) %>%
    dplyr::arrange(year)

print(se_compare, n = Inf)

# what ARE the between-stage ly correlations?
print(glmmTMB::VarCorr(fit_slope)$cond$ly_f) # 3x3, look at off-diagonals
attr(glmmTMB::VarCorr(fit_slope)$cond$ly_f, "correlation") # correlation form 

# also the stage-specific ly SDs is the motile one much smaller than chal/cope?
# (if so, the shared model was inflating motile ly variance by pooling)
sqrt(diag(glmmTMB::VarCorr(fit_slope)$cond$ly_f))

# Cmotile YEAR coefficients across the three fit
# with motile as the stage reference (levels mot/cope/chal),
# the year_f coefficients r the motile cell means directly in all three fits
yr_shared <- glmmTMB::fixef(tmb_fit)$cond[grep("^year_f", 
    names(glmmTMB::fixef(tmb_fit)$cond))]
yr_unstr  <- glmmTMB::fixef(fit_slope)$cond[grep("^year_f", 
    names(glmmTMB::fixef(fit_slope)$cond))]
yr_diag   <- glmmTMB::fixef(fit_diag)$cond[grep("^year_f", 
    names(glmmTMB::fixef(fit_diag)$cond))]

# SEs for each
se <- function(m) {
    v <- sqrt(diag(vcov(m)$cond))
    v[grep("^year_f", names(glmmTMB::fixef(m)$cond))]
}
se_shared <- se(tmb_fit); se_unstr <- se(fit_slope); se_diag <- se(fit_diag)

motile_compare <- data.frame(
    year = sub("year_f", "", names(yr_shared)),
    est_shared = round(yr_shared, 3),
    est_unstr = round(yr_unstr, 3),
    est_diff = round(yr_unstr - yr_shared, 3), # do points move?
    se_shared = round(se_shared, 3),
    se_unstr = round(se_unstr, 3),
    se_ratio_unstr = round(se_unstr / se_shared, 3), # the anti-conservatism 
    se_ratio_diag = round(se_diag / se_shared, 3) # is diagonal really <1?
)
print(motile_compare, row.names = FALSE)

# look at them
cat("\nmax |year point shift| (unstr vs shared):",
    round(max(abs(yr_unstr - yr_shared)), 3), "\n")
cat("SE ratio unstr/shared — range:",
    round(range(se_unstr / se_shared), 3), "\n")
cat("SE ratio diag/shared — range:",
    round(range(se_diag / se_shared), 3), "\n")

AIC(tmb_fit, fit_diag, fit_slope)

# is week RE shared or stage-specific? -----------------------------------------
f_wk_shared <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2, data = collated_df_long
)
f_wk_diag <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (0 + stage || week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2, data = collated_df_long
)
f_wk_unstr <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (0 + stage | week_f) + (0 + stage || ly_f),
    family = glmmTMB::nbinom2, data = collated_df_long
)

# converged?
lapply(list(shared = f_wk_shared, diag = f_wk_diag, unstr = f_wk_unstr),
       \(m) m$sdr$pdHess)

# between-stage WEEK correlations 
print(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f)
attr(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f, "correlation")
sqrt(diag(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f)) # stage-specific week SDs

# do motile year estimates/SEs move?
yr_s <- glmmTMB::fixef(f_wk_shared)$cond[grep("^year_f", names(glmmTMB::fixef(f_wk_shared)$cond))]
yr_u <- glmmTMB::fixef(f_wk_unstr)$cond[grep("^year_f", names(glmmTMB::fixef(f_wk_unstr)$cond))]
yr_d <- glmmTMB::fixef(f_wk_diag)$cond[grep("^year_f", names(glmmTMB::fixef(f_wk_diag)$cond))]

se <- function(m) {
    v <- sqrt(diag(vcov(m)$cond))
    v[grep("^year_f", names(glmmTMB::fixef(m)$cond))]
}
wk_compare <- data.frame(
    year = sub("year_f", "", names(yr_s)),
    est_shared = round(yr_s, 3),
    est_diff_unstr = round(yr_u - yr_s, 3),
    se_ratio_unstr = round(se(f_wk_unstr) / se(f_wk_shared), 3),
    se_ratio_diag = round(se(f_wk_diag) / se(f_wk_shared), 3)
)
print(wk_compare, row.names = FALSE)

cat("\nmax |year point shift| (wk unstr vs shared):",
    round(max(abs(yr_u - yr_s)), 3), "\n")
cat("SE ratio wk unstr/shared — range:",
    round(range(se(f_wk_unstr) / se(f_wk_shared)), 3), "\n")
cat("SE ratio wk diag/shared — range:",
    round(range(se(f_wk_diag) / se(f_wk_shared)), 3), "\n")

AIC(f_wk_shared, f_wk_diag, f_wk_unstr)

#  six fits predict plotted ----------------------------------------------------
# note: tmb_fit and f_wk_shared are the SAME model (shared week, and ly differs:
# tmb_fit has shared ly, f_wk_shared has diagonal ly). Label precisely.
fits <- list(
    `ly:shared  wk:shared`  = tmb_fit,      # (1|ly) + (1|week)
    `ly:unstr   wk:shared`  = fit_slope,    # (0+stage|ly) + (1|week)
    `ly:diag    wk:shared`  = fit_diag,     # (0+stage||ly) + (1|week)
    `ly:diag    wk:shared2` = f_wk_shared,  # same as fit_diag structurally
    `ly:diag    wk:diag`    = f_wk_diag,    # both diagonal  <- the candidate
    `ly:diag    wk:unstr`   = f_wk_unstr    # week unstructured
)

stage_levels <- levels(collated_df_long$stage)   # mot, cope, chal
year_levels  <- levels(collated_df_long$year_f)

# per-model, per-stage total RE variance (week + ly) for the sigma^2/2 term -----
#' Pulls the variance contributed by each RE for each stage. Shared RE: one
#' variance applied to all stages. Diagonal/unstructured: the stage-specific
#' diagonal entry of that RE's covariance matrix. Returns a named vec by stage.
re_var_by_stage <- function(fit, stages) {
    vc <- glmmTMB::VarCorr(fit)$cond
    # helper: variance this RE contributes to a given stage
    grp_var <- function(mat, stage) {
        if (is.null(mat)) return(0)
        d <- diag(as.matrix(mat))
        nm <- rownames(as.matrix(mat))
        if (length(d) == 1L && (is.null(nm) || !any(grepl("stage", nm)))) {
            # shared scalar intercept: one variance for all stages
            return(unname(d[1]))
        }
        # stage-specific: match the "stage<level>" diagonal entry
        key <- paste0("stage", stage)
        if (key %in% nm) return(unname(d[key]))
        # fallback: reference stage (motile) is the bare intercept in some
        # parameterizations — if absent, treat as the first diagonal
        return(unname(d[1]))
    }
    vapply(stages, function(s) {
        v_wk <- grp_var(vc$week_f, s)
        v_ly <- grp_var(vc$ly_f, s)
        v_wk + v_ly
    }, numeric(1))
}

# build predictions for one fit ------------------------------------------------
predict_one <- function(fit, label, stages, years) {
    grid <- expand.grid(
            year_f = factor(years, levels = years),
            stage = factor(stages, levels = stages),
            week_f = NA,   # NA => population-level for this grouping var (docs)
            ly_f = NA
        )
    pr <- predict(fit, newdata = grid, se.fit = TRUE, type = "link")
    pr <- predict(fit, newdata = grid, se.fit = TRUE,
                  re.form = NA, type = "link")  # population-level, link scale
    rev <- re_var_by_stage(fit, stages)         # sigma^2 total per stage

    grid$eta <- pr$fit
    grid$se  <- pr$se.fit
    grid$half_s2 <- rev[as.character(grid$stage)] / 2   # sigma^2 / 2 per stage

    # mean on response scale: exp(eta + sigma^2/2); CI from fixed-effect SE only
    grid$mean <- exp(grid$eta + grid$half_s2)
    grid$lo   <- exp(grid$eta - 1.96 * grid$se + grid$half_s2)
    grid$hi   <- exp(grid$eta + 1.96 * grid$se + grid$half_s2)
    grid$model <- label
    grid$year  <- as.integer(as.character(grid$year_f))
    grid[, c("model", "year", "stage", "mean", "lo", "hi")]
}

pred_all <- do.call(rbind, Map(
    predict_one, fits, names(fits),
    MoreArgs = list(stages = stage_levels, years = year_levels)
))

sapply(list(orig = tmb_fit, best = f_wk_diag),
       \(f) re_var_by_stage(f, stage_levels))

# reproduce the exact subsample rows the builder used (seed = 1)
# set.seed(1)
# keep <- sort(sample.int(nrow(collated_df_long),
#                         floor(0.25 * nrow(collated_df_long))))
# sub_df <- collated_df_long[keep, ] |>
#     droplevels()

# f_sub <- glmmTMB::glmmTMB(
#     count ~ 0 + year_f + stage + (0 + stage || week_f) + (0 + stage || ly_f),
#     family = glmmTMB::nbinom2, data = sub_df
# )
# sqrt(diag(glmmTMB::VarCorr(f_sub)$cond$week_f))  # motile week SD, no prior

# plot: facet by stage, colour by model ---------------------------------------
pd <- position_dodge(width = 0.6)

p <- ggplot(pred_all, aes(year, mean, colour = model)) +
    geom_errorbar(
        aes(ymin = lo, ymax = hi),
        width = 0, linewidth = 0.5, position = pd
    ) +
    geom_point(size = 1.6, position = pd) +
    facet_wrap(~ stage, scales = "free_y", ncol = 1) +
    labs(
        x = "Year", y = "Mean predicted lice per fish",
        colour = "RE structure"
    ) +
    theme_better()

save_fig(
    p,
    name = "predicted-lice-per-fish-by-model",
    dir = here::here("./figs/model-comparison"),
    width = 9, height = 10,
    caption = paste(
        "Mean predicted lice per fish per year (population-level, random",
        "effects integrated out via exp(eta + sigma^2/2)) for six random-effect",
        "structures, faceted by louse stage. Ribbons are 95% CIs from",
        "fixed-effect (year x stage) uncertainty only; the sigma^2/2 mean",
        "correction is applied at the point estimate of the variance",
        "components and its uncertainty is NOT propagated. Motile is the",
        "reported stage. The 'ly:diag wk:diag' model is the candidate",
        "(both REs stage-specific); 'ly:shared wk:shared' is the original."
    )
)

pred_two <- pred_all[pred_all$model %in%
    c("ly:shared  wk:shared", "ly:diag    wk:diag"), ]

# relabel for a clean two-panel header, and order original -> best
pred_two$panel <- factor(
    ifelse(grepl("shared  wk:shared", pred_two$model),
           "Original (both shared)", "Best (both stage-specific)"),
    levels = c("Original (both shared)", "Best (both stage-specific)")
)

p2 <- ggplot(pred_two, aes(year, mean, colour = stage)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5) +
    geom_point(size = 1.6) +
    facet_grid(stage ~ panel, scales = "free_y") +
    labs(
        x = "Year", y = "Mean predicted lice per fish",
        colour = "Stage"
    ) +
    theme_better()

save_fig(
    p2,
    name = "predicted-lice-original-vs-best",
    dir = here::here("./figs/model-comparison"),
    width = 10, height = 9,
    caption = paste(
        "Mean predicted lice per fish per year (population-level, random",
        "effects integrated out via exp(eta + sigma^2/2)), comparing the",
        "original model (shared week and location-year random effects) with the",
        "best-supported model (stage-specific diagonal week and location-year",
        "REs; see decisions E16/E17). Rows are louse stages, columns are the two",
        "models. Points are yearly means; bars are 95% CIs from fixed-effect",
        "(year x stage) uncertainty only, with the sigma^2/2 mean correction",
        "applied at the point estimate of the variance components (its",
        "uncertainty not propagated). The motile row (top) is the reported stage:",
        "note the downward shift and widened intervals in the best model."
    )
)