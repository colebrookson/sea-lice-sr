#' DESCRIPTION: frequentist tests for my stage-structure question. Four
#' fits spanning the progression from Andrew's shared-RE three-stage
#' model to motile-only
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)

collated_df_long <- qs2::qs_read(
  paste0(
    here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.qs2"
  )
)

#' 2001 has no cope rows, so one combination is empty and must be
#' dropped
collated_df_long <- collated_df_long %>%
  dplyr::mutate(
    ys_f = droplevels(interaction(year_f, stage, sep = "."))
  )

n_yr <- nlevels(collated_df_long$year_f)
n_st <- nlevels(collated_df_long$stage)
stopifnot(nlevels(collated_df_long$ys_f) == (n_yr * n_st) - 1)

mot_df <- collated_df_long %>%
  dplyr::filter(stage == "mot") %>%
  dplyr::mutate(
    year_f = droplevels(year_f),
    week_f = droplevels(week_f),
    ly_f = droplevels(ly_f)
  )

# published w/ shared scalar REs -----------------------------------------------
f_shared <- glmmTMB::glmmTMB(
  count ~ 0 + year_f + stage + (1 | week_f) + (1 | ly_f),
  family = glmmTMB::nbinom2,
  data = collated_df_long
)

# diagonal stage-specific REs, additive year + stage ---------------------------
#' use diag() bc || splits formula TERMS, and `0 + stage` is a single
#' term, so it does not reliably give one variance per stage level
f_diag_add <- glmmTMB::glmmTMB(
  count ~ 0 +
    year_f +
    stage +
    diag(0 + stage | week_f) +
    diag(0 + stage | ly_f),
  family = glmmTMB::nbinom2,
  data = collated_df_long
)

# diagonal REs, year x stage cell means ----------------------------------------
f_diag_int <- glmmTMB::glmmTMB(
  count ~ 0 + ys_f + diag(0 + stage | week_f) + diag(0 + stage | ly_f),
  family = glmmTMB::nbinom2,
  data = collated_df_long
)

# motile only, scalar REs, no stage term ---------------------------------------
f_mot <- glmmTMB::glmmTMB(
  count ~ 0 + year_f + (1 | week_f) + (1 | ly_f),
  family = glmmTMB::nbinom2,
  data = mot_df
)

# AIC (valid here, same response vector) ---------------------------------------
aic_tab <- data.frame(
  fit = c("F1_shared", "F2_diag_add", "F3_diag_int"),
  df = sapply(list(f_shared, f_diag_add, f_diag_int), \(m) {
    attr(logLik(m), "df")
  }),
  aic = sapply(list(f_shared, f_diag_add, f_diag_int), AIC)
)
aic_tab$delta <- aic_tab$aic - min(aic_tab$aic)
print(aic_tab)

#' NOT AIC bc different response vector, non-comparable likelihoods ------------
#' both are log-scale motile cell means, so they compare element-wise
yr_levels <- levels(mot_df$year_f)

b_f3 <- glmmTMB::fixef(f_diag_int)$cond
b_f3_mot <- b_f3[paste0("ys_f", yr_levels, ".mot")]

b_f4 <- glmmTMB::fixef(f_mot)$cond
b_f4_mot <- b_f4[paste0("year_f", yr_levels)]

stopifnot(!any(is.na(b_f3_mot)), !any(is.na(b_f4_mot)))

year_cmp <- data.frame(
  year = yr_levels,
  f3_motile = as.numeric(b_f3_mot),
  f4_motile = as.numeric(b_f4_mot)
)
year_cmp$diff_log <- year_cmp$f3_motile - year_cmp$f4_motile
year_cmp$ratio <- exp(year_cmp$diff_log)
print(year_cmp)

#' max absolute discrepancy under ~0.1 log units means the three-stage
#' contributes nothing to the reported quantity and motile-only is simpler
max(abs(year_cmp$diff_log))
max(exp(abs(year_cmp$diff_log)))

# dispersion and RE scales, for the record ------------------------------------
print(sapply(fits, glmmTMB::sigma))

print(c(
  F2 = attr(glmmTMB::VarCorr(f_diag_add)$cond$week_f, "stddev")["stagemot"],
  F3 = attr(glmmTMB::VarCorr(f_diag_int)$cond$week_f, "stddev")["stagemot"],
  F4 = attr(glmmTMB::VarCorr(f_mot)$cond$week_f, "stddev")[1]
))


b_f2_mot <- glmmTMB::fixef(f_diag_add)$cond[paste0("year_f", yr_levels)]
max(abs(as.numeric(b_f2_mot) - as.numeric(b_f4_mot)))
