#' DESCRIPTION: here I'm going to write out the relatively simple GLMM that
#' we've used before for these data so I can fit the model. Both week and
#' location-year random effects are now DIAGONAL stage-specific:
#' each stage gets its own variance and its own per-column sum-to-zero.

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)
library(nimble)
library(nimbleHMC)

collated_df_long <- qs2::qs_read(
  paste0(
    here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.qs2"
  )
)

# make the model itself --------------------------------------------------------
glmm_mod <- nimble::nimbleCode({
  # year as cell means -------------------------------------------------------
  for (k in 1:Yr) {
    beta_year[k] ~ dnorm(0, sd = 1)
  }
  beta_stage[1] <- 0
  for (s in 2:S) {
    beta_stage[s] ~ dnorm(0, sd = 1.5)
  }

  # overdispersion (confirmed via prior predictive check)
  r ~ dgamma(shape = 1, rate = 0.5)

  # week RE: DIAGONAL stage-specific  ----------------------------------------
  # motile week SD ~2.1, so sigma prior widened from 0.5 to 3 to admit it
  for (s in 1:S) {
    sigma_week_raw[s] ~ dnorm(0, sd = 3)
    sigma_week[s] <- abs(sigma_week_raw[s])
  }
  for (s in 1:S) {
    for (i in 1:W) {
      z_week_raw[i, s] ~ dnorm(0, sd = 1)
    }
    z_week_mean[s] <- sum(z_week_raw[1:W, s]) / W
    for (i in 1:W) {
      b_week[i, s] <- sigma_week[s] * (z_week_raw[i, s] - z_week_mean[s])
    }
  }

  # location-year RE: DIAGONAL stage-specific --------------------------------
  # one sigma per stage, one sum-to-zero per stage column; prior widened to 1
  for (s in 1:S) {
    sigma_ly_raw[s] ~ dnorm(0, sd = 1)
    sigma_ly[s] <- abs(sigma_ly_raw[s])
  }
  for (s in 1:S) {
    for (j in 1:L) {
      z_ly_raw[j, s] ~ dnorm(0, sd = 1)
    }
    z_ly_mean[s] <- sum(z_ly_raw[1:L, s]) / L
    for (j in 1:L) {
      b_ly[j, s] <- sigma_ly[s] * (z_ly_raw[j, s] - z_ly_mean[s])
    }
  }

  # likelihood ---------------------------------------------------------------
  for (i in 1:N) {
    log(mu[i]) <- beta_year[year_idx[i]] +
      beta_stage[stage_idx[i]] +
      b_week[week_idx[i], stage_idx[i]] +
      b_ly[ly_idx[i], stage_idx[i]] # both REs stage-indexed
    p[i] <- r / (r + mu[i])
    Y[i] ~ dnegbin(p[i], r)
  }
})

# data ! -----------------------------------------------------------------------
# level counts
Yr <- nlevels(collated_df_long$year_f)
S <- nlevels(collated_df_long$stage)
W <- nlevels(collated_df_long$week_f)
L <- nlevels(collated_df_long$ly_f)

# max index must equal declared level count
stopifnot(
  max(collated_df_long$year_idx) == Yr,
  max(collated_df_long$stage_idx) == S,
  max(collated_df_long$week_idx) == W,
  max(collated_df_long$ly_idx) == L,
  collated_df_long$stage_idx[collated_df_long$stage == "mot"][1] == 1
)

consts <- list(
  N = nrow(collated_df_long),
  Yr = Yr,
  S = S,
  W = W,
  L = L,
  year_idx = collated_df_long$year_idx,
  stage_idx = collated_df_long$stage_idx,
  week_idx = collated_df_long$week_idx,
  ly_idx = collated_df_long$ly_idx
)
data_list <- list(Y = collated_df_long$count)

# ok now do a ppc --------------------------------------------------------------
#' Updated to the DIAGONAL structure: stage-specific week/ly sigmas at the new
#' prior widths with per-stage centered RE draws
prior_predictive <- function(n_sim = 500, dat = collated_df_long) {
  obs_zero <- mean(dat$count == 0)
  obs_q <- quantile(dat$count, c(0.5, 0.9, 0.99, 1))

  sim_zero <- numeric(n_sim)
  sim_max <- numeric(n_sim)
  for (s in seq_len(n_sim)) {
    r_s <- rgamma(1, shape = 1, rate = 0.5)
    by <- rnorm(Yr, 0, 1)
    bs <- c(0, rnorm(S - 1, 0, 1.5))

    # stage-specific week + ly sigmas at the NEW prior widths
    sig_wk <- abs(rnorm(S, 0, 1.5))
    sig_ly <- abs(rnorm(S, 0, 1))

    # per-stage RE draws, centered (redundant sum-to-zero)
    bw <- sapply(seq_len(S), \(st) {
      z <- rnorm(W)
      sig_wk[st] * (z - mean(z))
    }) # W x S
    bl <- sapply(seq_len(S), \(st) {
      z <- rnorm(L)
      sig_ly[st] * (z - mean(z))
    }) # L x S

    eta <- by[dat$year_idx] +
      bs[dat$stage_idx] +
      bw[cbind(dat$week_idx, dat$stage_idx)] +
      bl[cbind(dat$ly_idx, dat$stage_idx)]
    y <- rnbinom(length(eta), size = r_s, mu = exp(eta))
    sim_zero[s] <- mean(y == 0)
    sim_max[s] <- max(y)
  }
  list(
    obs_prop_zero = obs_zero,
    sim_prop_zero = quantile(sim_zero, c(0.025, 0.5, 0.975)),
    obs_quantiles = obs_q,
    sim_max = quantile(sim_max, c(0.5, 0.975, 1))
  )
}

# ppc <- prior_predictive()
# print(ppc)
#' watch sim_max: widening week sigma to 3 pushes the tail up. If sim_max blows
#' past ~1e4-1e5, back week sigma down to 2 (still admits the motile ~2.1 at
#' ~1 SD). Want sim_prop_zero to bracket observed 0.86 and sim_max in the
#' thousands, not millions.

# set up the config/compile ----------------------------------------------------
make_inits <- function() {
  list(
    beta_year = rnorm(Yr, 0, 1),
    beta_stage = c(0, rnorm(S - 1, 0, 1)),
    r = rgamma(1, 2, 1),
    sigma_week_raw = rnorm(S, 0, 1.5),
    z_week_raw = matrix(rnorm(W * S, 0, 0.5), nrow = W, ncol = S),
    sigma_ly_raw = rnorm(S, 0, 0.5),
    z_ly_raw = matrix(rnorm(L * S, 0, 0.5), nrow = L, ncol = S)
  )
}

# nimbleOptions(buildModelDerivs = TRUE)
# model <- nimbleModel(
#     glmm_mod, constants = consts, data = data_list, inits = make_inits(),
#     buildDerivs = TRUE, calculate = FALSE
# )
# cmodel <- compileNimble(model)

# monitors <- c("beta_year", "beta_stage", "r", "sigma_week", "sigma_ly")

# conf <- configureHMC(model, monitors = monitors)
# mcmc <- buildMCMC(conf)
# cmcmc <- compileNimble(mcmc, project = model)

# here goes nothin!
# samples <- runMCMC(
#     cmcmc,
#     niter = 600, nburnin = 300, nchains = 4,
#     inits = replicate(4, make_inits(), simplify = FALSE),
#     samplesAsCodaMCMC = TRUE, setSeed = 1:4
# )

# parallelized version! --------------------------------------------------------
run_one_chain <- function(
  seed,
  glmm_mod,
  consts,
  data,
  monitors,
  dims,
  log_dir
) {
  # open a per-chain log; capture BOTH stdout (progress bar) and messages
  log_file <- file.path(log_dir, paste0("chain_", seed, ".log"))
  con <- file(log_file, open = "wt")
  sink(con, split = FALSE)
  sink(con, type = "message")
  on.exit(
    {
      sink(type = "message")
      sink()
      close(con)
    },
    add = TRUE
  )

  ts <- function(msg) cat(format(Sys.time(), "%H:%M:%S"), msg, "\n")

  library(nimble)
  library(nimbleHMC)
  Yr <- dims$Yr
  S <- dims$S
  W <- dims$W
  L <- dims$L
  make_inits <- function() {
    list(
      beta_year = rnorm(Yr, 0, 1),
      beta_stage = c(0, rnorm(S - 1, 0, 1)),
      r = rgamma(1, 2, 1),
      sigma_week_raw = rnorm(S, 0, 1.5),
      z_week_raw = matrix(rnorm(W * S, 0, 0.5), nrow = W, ncol = S),
      sigma_ly_raw = rnorm(S, 0, 0.5),
      z_ly_raw = matrix(rnorm(L * S, 0, 0.5), nrow = L, ncol = S)
    )
  }
  nimbleOptions(buildModelDerivs = TRUE)

  ts("building model")
  m <- nimbleModel(
    glmm_mod,
    constants = consts,
    data = data,
    inits = make_inits(),
    buildDerivs = TRUE,
    calculate = FALSE
  )
  ts("compiling model")
  cm <- compileNimble(m)
  conf <- configureHMC(m, monitors = monitors, control = list(maxTreeDepth = 7))
  mcmc <- buildMCMC(conf)
  ts("compiling mcmc")
  cmcmc <- compileNimble(mcmc, project = m)

  ts("sampling start")
  t0 <- Sys.time()
  out <- runMCMC(
    cmcmc,
    niter = 2000,
    nburnin = 1000,
    setSeed = seed,
    samplesAsCodaMCMC = TRUE,
    progressBar = TRUE
  )
  attr(out, "elapsed") <- Sys.time() - t0
  ts("sampling done")
  return(out)
}

# cl <- parallel::makeCluster(4)
# samples_list <- parallel::parLapply(
#     cl, 1:4, run_one_chain,
#     glmm_mod = glmm_mod, consts = consts, data = data_list,
#     monitors = monitors,
#     dims = list(Yr = Yr, S = S, W = W, L = L)
# )
# parallel::stopCluster(cl)

# # super quick check here:
# samples <- coda::as.mcmc.list(samples_list)
# coda::gelman.diag(samples, multivariate = FALSE)
# coda::effectiveSize(samples)
# summary(samples[, "r"]) # should center ~0.55 per glmmTMB
#' also check sigma_week[1] (motile) lands near 2.1 — if the prior shrank it
#' toward 0.5, the fix didn't take and the bias is back.

#  subsample for fast iteration ------------------------------------------------
#' Draw a row-fraction of the long frame and rebuild ALL objects from
#' the subsample
build_nimble_inputs <- function(df_long, frac = 1, seed = 1) {
  set.seed(seed)

  # row-drop FIRST, then rebuild factors so levels stay contiguous
  if (frac < 1) {
    keep <- sample.int(nrow(df_long), floor(frac * nrow(df_long)))
    df_long <- df_long[sort(keep), ]
  }
  df_long <- df_long %>%
    dplyr::mutate(
      stage = droplevels(stage),
      year_f = droplevels(year_f),
      week_f = droplevels(week_f),
      ly_f = droplevels(ly_f)
    )

  # re-derive indices from the (possibly) reduced factors
  df_long <- df_long %>%
    dplyr::mutate(
      year_idx = as.integer(year_f),
      week_idx = as.integer(week_f),
      stage_idx = as.integer(stage),
      ly_idx = as.integer(ly_f)
    )

  Yr <- nlevels(df_long$year_f)
  S <- nlevels(df_long$stage)
  W <- nlevels(df_long$week_f)
  L <- nlevels(df_long$ly_f)

  # subsample can silently drop a whole level — catch it loudly
  stopifnot(
    max(df_long$year_idx) == Yr,
    max(df_long$stage_idx) == S,
    max(df_long$week_idx) == W,
    max(df_long$ly_idx) == L,
    df_long$stage_idx[df_long$stage == "mot"][1] == 1,
    # motile must still be reference after droplevels
    levels(df_long$stage)[1] == "mot"
  )

  # level maps rebuilt from the subsample (guard off-by-one on plots)
  level_maps <- list(
    year = tibble::tibble(idx = seq_len(Yr), year = levels(df_long$year_f)),
    week = tibble::tibble(idx = seq_len(W), week = levels(df_long$week_f)),
    stage = tibble::tibble(idx = seq_len(S), stage = levels(df_long$stage)),
    ly = tibble::tibble(idx = seq_len(L), ly = levels(df_long$ly_f))
  )

  consts <- list(
    N = nrow(df_long),
    Yr = Yr,
    S = S,
    W = W,
    L = L,
    year_idx = df_long$year_idx,
    stage_idx = df_long$stage_idx,
    week_idx = df_long$week_idx,
    ly_idx = df_long$ly_idx
  )
  data_list <- list(Y = df_long$count)

  list(
    consts = consts,
    data_list = data_list,
    dims = list(Yr = Yr, S = S, W = W, L = L),
    level_maps = level_maps,
    n_rows = nrow(df_long)
  )
}

# build a 25% subsample -------------------------------------------------------
# sub <- build_nimble_inputs(collated_df_long, frac = 0.25, seed = 1)
# cat("subsample rows:", sub$n_rows,
#     "| Yr", sub$dims$Yr, "S", sub$dims$S,
#     "W", sub$dims$W, "L", sub$dims$L, "\n")

# build a "subsample" but it's the whole thing ---------------------------------
sub <- build_nimble_inputs(collated_df_long, frac = 1, seed = 1)
cat(
  "subsample rows:",
  sub$n_rows,
  "| Yr",
  sub$dims$Yr,
  "S",
  sub$dims$S,
  "W",
  sub$dims$W,
  "L",
  sub$dims$L,
  "\n"
)

# parallel HMC on the subsample ------------------------------------------------
monitors <- c("beta_year", "beta_stage", "r", "sigma_week", "sigma_ly")

cl <- parallel::makeCluster(4)
samples_list <- parallel::parLapply(
  cl,
  1:4,
  run_one_chain,
  glmm_mod = glmm_mod,
  consts = consts,
  data = data_list,
  monitors = monitors,
  dims = list(Yr = Yr, S = S, W = W, L = L),
  log_dir = log_dir
)
parallel::stopCluster(cl)

samples <- coda::as.mcmc.list(samples_list)
qs2::qs_save(
  samples_list,
  paste0(
    here::here("./data/scfs-data/clean/"),
    "glmm-diagonal-full-samples.qs2"
  )
)
samples_list <- qs2::qs_read(
  paste0(
    here::here("./data/scfs-data/clean/"),
    "glmm-diagonal-full-samples.qs2"
  )
)


# per-chain wall-clock (the whole point of this run)
sapply(samples_list, \(x) as.numeric(attr(x, "elapsed"), units = "mins"))

coda::gelman.diag(samples, multivariate = FALSE)
coda::effectiveSize(samples)
summary(samples[, "r"]) # ~0.55
summary(samples[, "sigma_week[1]"]) # must reach ~2.1 (full data)
summary(samples[, "sigma_ly[1]"]) # motile ly, sanity

# full set of diagnostics ------------------------------------------------------
