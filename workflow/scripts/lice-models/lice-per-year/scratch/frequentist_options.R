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

# CHECK 1: between-stage WEEK correlations (the decisive number)
print(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f)
attr(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f, "correlation")
sqrt(diag(glmmTMB::VarCorr(f_wk_unstr)$cond$week_f)) # stage-specific week SDs

# CHECK 2: do motile year estimates/SEs move?
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