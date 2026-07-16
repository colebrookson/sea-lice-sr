#' DESCRIPTION: here I'm going to write out the relatively simple GLMM that 
#' we've used before for these data so I can fit the model 

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)
library(nimble)
library(nimbleHMC)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
)

# get rid of the weeks we don't want here
collated_df_long <- collated_df %>%
    dplyr::filter(week %notin% c(9, 28, 33)) %>%
    tidyr::pivot_longer(
        cols = c(lep_mot, lep_cope, lep_chal),
        names_to = "stage",
        names_prefix = "lep_", # so values are mot/cope/chal
        values_to = "count"
    ) %>%
    # drop the 2001 cope NA rows (only NA cell, per the audit)
    dplyr::filter(!is.na(count)) %>%
    dplyr::select(obs_id, count, stage, year, week, location) %>%
    dplyr::mutate(
        # motile first so stage_idx == 1 is the reference 
        stage = factor(stage, levels = c("mot", "cope", "chal")),
        # location-year from observed combinations only 
        ly = factor(paste(location, year, sep = "_")),
        # factor the grouping vars AFTER 
        year_f = droplevels(factor(year)),
        week_f = droplevels(factor(week)),
        ly_f = droplevels(ly)
    )

# integer index vectors for NIMBLE
collated_df_long <- collated_df_long %>%
    dplyr::mutate(
        year_idx = as.integer(year_f),
        week_idx = as.integer(week_f),
        stage_idx = as.integer(stage),
        ly_idx = as.integer(ly_f)
    )

# level maps — idx -> label, so posteriors map back
year_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$year_f)),
    year = levels(collated_df_long$year_f)
)
week_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$week_f)),
    week = levels(collated_df_long$week_f)
)
stage_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$stage)),
    stage = levels(collated_df_long$stage)
)
ly_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$ly_f)),
    ly = levels(collated_df_long$ly_f)
)
# a couple quick data checks ---------------------------------------------------

# check this has all been done right with the following: 
# collated_df_long %>% dplyr::count(stage) # no NAs
# collated_df_long %>% dplyr::count(year, stage) %>% # only 2001 is empty at cope
#     tidyr::pivot_wider(names_from = stage, values_from = n, values_fill = 0)
# collated_df_long %>% dplyr::count(ly_f) %>% dplyr::arrange(n) # how many loc-yr?
# # how many distinct locations, and are any suspiciously near-duplicates?
# collated_df_long %>% dplyr::distinct(location) %>% dplyr::arrange(location)

# # and the site x year grid — is 75 a clean function of sites x years-observed?
# collated_df_long %>% dplyr::count(location, year) %>%
#     tidyr::pivot_wider(names_from = year, values_from = n, values_fill = 0) %>%
#     print(width = Inf)

# make the model itself --------------------------------------------------------
glmm_mod <- nimble::nimbleCode({
    # year as cell means, no intercept -----------------------------------------
    for (k in 1:Yr) {
        beta_year[k] ~ dnorm(0, sd = 1)
    }
    beta_stage[1] <- 0
    for (s in 2:S) {
        beta_stage[s] ~ dnorm(0, sd = 1.5)
    }

    # overdispersion (confirm via prior predictive check) 
    r ~ dgamma(shape = 1, rate = 0.5)

    # RE scales: half-normal(0,1) via folded normal 
    sigma_week_raw ~ dnorm(0, sd = 0.5)
    sigma_week <- abs(sigma_week_raw)
    sigma_ly_raw ~ dnorm(0, sd = 0.5)
    sigma_ly <- abs(sigma_ly_raw)

    # week RE: non-centered + redundant sum-to-zero  
    for (i in 1:W) {
        z_week_raw[i] ~ dnorm(0, sd = 1)
    }
    z_week_mean <- sum(z_week_raw[1:W]) / W
    for (i in 1:W) {
        b_week[i] <- sigma_week * (z_week_raw[i] - z_week_mean)
    }

    # location-year RE: non-centered 
    for (j in 1:L) {
        z_ly_raw[j] ~ dnorm(0, sd = 1)
    }
    z_ly_mean <- sum(z_ly_raw[1:L]) / L
    for (j in 1:L) {
        b_ly[j] <- sigma_ly * (z_ly_raw[j] - z_ly_mean)
    }

    # likelihood ---------------------------------------------------------------
    for (i in 1:N) {
        log(mu[i]) <- beta_year[year_idx[i]] + beta_stage[stage_idx[i]] +
            b_week[week_idx[i]] + b_ly[ly_idx[i]]
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
    N = nrow(collated_df_long), Yr = Yr, S = S, W = W, L = L,
    year_idx = collated_df_long$year_idx,
    stage_idx = collated_df_long$stage_idx,
    week_idx = collated_df_long$week_idx,
    ly_idx = collated_df_long$ly_idx
)
data_list <- list(Y = collated_df_long$count)

# ok now do a ppc --------------------------------------------------------------
prior_predictive <- function(n_sim = 500, dat = collated_df_long) {
    obs_zero <- mean(dat$count == 0)
    obs_q <- quantile(dat$count, c(0.5, 0.9, 0.99, 1))

    sim_zero <- numeric(n_sim)
    sim_max <- numeric(n_sim)
    for (s in seq_len(n_sim)) {
        r_s <- rgamma(1, shape = 1, rate = 0.5)
        by <- rnorm(Yr, 0, 1)
        bs <- c(0, rnorm(S - 1, 0, 1.5))
        sig_wk <- abs(rnorm(1, 0, 0.5))
        sig_ly <- abs(rnorm(1, 0, 0.5))
        zw <- rnorm(W)
        bw <- sig_wk * (zw - mean(zw))
        zl <- rnorm(L)
        bl <- sig_ly * (zl - mean(zl))
        mu <- exp(
            by[dat$year_idx] + bs[dat$stage_idx] +
                bw[dat$week_idx] + bl[dat$ly_idx]
        )
        y <- rnbinom(length(mu), size = r_s, mu = mu)
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

ppc <- prior_predictive()
print(ppc)

# set up the config/compile ----------------------------------------------------
make_inits <- function() list(
    beta_year = rnorm(Yr, 0, 1),
    beta_stage = c(0, rnorm(S - 1, 0, 1)),
    r = rgamma(1, 2, 1),
    sigma_week_raw = rnorm(1, 0, 0.5),
    sigma_ly_raw = rnorm(1, 0, 0.5),
    z_week_raw = rnorm(W, 0, 0.5),
    z_ly_raw = rnorm(L, 0, 0.5)
)

nimbleOptions(buildModelDerivs = TRUE)
model <- nimbleModel(
    glmm_mod, constants = consts, data = data_list, inits = make_inits(),
    buildDerivs = TRUE, calculate = FALSE
)
cmodel <- compileNimble(model)

monitors <- c("beta_year", "beta_stage", "r", "sigma_week", "sigma_ly")

conf <- configureHMC(model, monitors = monitors)
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = model)

# here goes nothin!
# samples <- runMCMC(
#     cmcmc,
#     niter = 600, nburnin = 300, nchains = 4,
#     inits = replicate(4, make_inits(), simplify = FALSE),
#     samplesAsCodaMCMC = TRUE, setSeed = 1:4
# )

# parallelized version! --------------------------------------------------------
run_one_chain <- function(seed, glmm_mod, consts, data, monitors, dims) {
    library(nimble); library(nimbleHMC)
    Yr <- dims$Yr; S <- dims$S; W <- dims$W; L <- dims$L
    make_inits <- function() list(
        beta_year = rnorm(Yr, 0, 1),
        beta_stage = c(0, rnorm(S - 1, 0, 1)),
        r = rgamma(1, 2, 1),
        sigma_week_raw = rnorm(1, 0, 0.5),
        sigma_ly_raw = rnorm(1, 0, 0.5),
        z_week_raw = rnorm(W, 0, 0.5),
        z_ly_raw = rnorm(L, 0, 0.5)
    )
    nimbleOptions(buildModelDerivs = TRUE)
    m <- nimbleModel(glmm_mod, constants = consts, data = data,
                     inits = make_inits(), buildDerivs = TRUE, 
                     calculate = FALSE)
    cm <- compileNimble(m)
    conf <- configureHMC(m, monitors = monitors, 
    control = list(maxTreeDepth = 7))
    mcmc <- buildMCMC(conf)
    cmcmc <- compileNimble(mcmc, project = m)
    t0 <- Sys.time()
    out <- runMCMC(cmcmc, niter = 600, nburnin = 300, setSeed = seed,
                   samplesAsCodaMCMC = TRUE)
    attr(out, "elapsed") <- Sys.time() - t0
    return(out)
}

cl <- parallel::makeCluster(4)
samples_list <- parallel::parLapply(
    cl, 1:4, run_one_chain,
    glmm_mod = glmm_mod, consts = consts, data = data_list,
    monitors = monitors,
    dims = list(Yr = Yr, S = S, W = W, L = L)
)
parallel::stopCluster(cl)

# super quick check here:
samples <- coda::as.mcmc.list(samples_list)
coda::gelman.diag(samples, multivariate = FALSE)
coda::effectiveSize(samples)
summary(samples[, "r"])  # should center ~0.55 per glmmTMB

#  subsample for fast iteration ------------------------------------------------
#' Draw a row-fraction of the long frame and rebuild ALL downstream objects from
#' the subsample. Returns everything the parallel fit needs
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
        N = nrow(df_long), Yr = Yr, S = S, W = W, L = L,
        year_idx = df_long$year_idx,
        stage_idx = df_long$stage_idx,
        week_idx = df_long$week_idx,
        ly_idx = df_long$ly_idx
    )
    data_list <- list(Y = df_long$count)

    list(
        consts = consts, data_list = data_list,
        dims = list(Yr = Yr, S = S, W = W, L = L),
        level_maps = level_maps, n_rows = nrow(df_long)
    )
}

# build a 25% subsample -------------------------------------------------------
sub <- build_nimble_inputs(collated_df_long, frac = 0.25, seed = 1)
cat("subsample rows:", sub$n_rows,
    "| Yr", sub$dims$Yr, "S", sub$dims$S,
    "W", sub$dims$W, "L", sub$dims$L, "\n")

# parallel HMC on the subsample ------------------------------------------------
monitors <- c("beta_year", "beta_stage", "r", "sigma_week", "sigma_ly")

cl <- parallel::makeCluster(4)
samples_list <- parallel::parLapply(
    cl, 1:4, run_one_chain,
    glmm_mod = glmm_mod, consts = sub$consts, data = sub$data_list,
    monitors = monitors, dims = sub$dims
)
parallel::stopCluster(cl)

samples <- coda::as.mcmc.list(samples_list)

# per-chain wall-clock (the whole point of this run)
sapply(samples_list, \(x) as.numeric(attr(x, "elapsed"), units = "mins"))

coda::gelman.diag(samples, multivariate = FALSE)
coda::effectiveSize(samples)
summary(samples[, "r"])   # ~0.55; subsample widens the SD but mean should hold

# Metropolis comparison (subsample) --------------------------------------------
Yr <- sub$dims$Yr; S <- sub$dims$S; W <- sub$dims$W; L <- sub$dims$L

make_inits <- function() list(
    beta_year = rnorm(Yr, 0, 1),
    beta_stage = c(0, rnorm(S - 1, 0, 1)),
    r = rgamma(1, 2, 1),
    sigma_week_raw = rnorm(1, 0, 0.5),
    sigma_ly_raw = rnorm(1, 0, 0.5),
    z_week_raw = rnorm(W, 0, 0.5),
    z_ly_raw = rnorm(L, 0, 0.5)
)

m <- nimbleModel(glmm_mod, constants = sub$consts, data = sub$data_list,
                 inits = make_inits(), calculate = FALSE)
cm <- compileNimble(m)
conf <- configureMCMC(m, monitors = monitors) # default RW samplers
mcmc <- buildMCMC(conf)
cmcmc <- compileNimble(mcmc, project = m)

samples_rw <- runMCMC(
    cmcmc,
    niter = 600, nburnin = 300, nchains = 4,
    inits = replicate(4, make_inits(), simplify = FALSE),
    samplesAsCodaMCMC = TRUE, setSeed = 1:4,
    progressBar = TRUE
)

coda::gelman.diag(samples_rw, multivariate = FALSE)
coda::effectiveSize(samples_rw)
summary(samples_rw[, "r"])

# comparison to frequentist ----------------------------------------------------
# same data, same structure — factors straight from the long frame
tmb_fit <- glmmTMB::glmmTMB(
    count ~ 0 + year_f + stage + (1 | week_f) + (1 | ly_f),
    family = glmmTMB::nbinom2,
    data = collated_df_long
)

summary(tmb_fit)

# pull the pieces to compare against NIMBLE
glmmTMB::fixef(tmb_fit)$cond # year_f coefs = beta_year; stage = beta_stage
sigma(tmb_fit) # this is glmmTMB's phi = r (nbinom2 dispersion)
# RE sds
print(glmmTMB::VarCorr(tmb_fit))

# full set of diagnostics ------------------------------------------------------