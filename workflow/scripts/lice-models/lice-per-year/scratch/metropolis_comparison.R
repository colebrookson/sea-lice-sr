#' DESCRIPTION: here I want to see if a normal RW MCMC works ok?

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)
library(nimble)
library(nimbleHMC)

collated_df_long <- readr::read_csv(
    paste0(here::here("./data/scfs-data/clean/"),
    "lice-counts-long-form-for-regression.csv")
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

    # week RE scale ------------------------------------------------------------
    sigma_week_raw ~ dnorm(0, sd = 0.5)
    sigma_week <- abs(sigma_week_raw)

    # week RE ------------------------------------------------------------------
    for (i in 1:W) {
        z_week_raw[i] ~ dnorm(0, sd = 1)
    }
    z_week_mean <- sum(z_week_raw[1:W]) / W
    for (i in 1:W) {
        b_week[i] <- sigma_week * (z_week_raw[i] - z_week_mean)
    }

    # location-year RE: DIAGONAL stage-specific (E16) --------------------------
    # one sigma per stage which should be one sum-to-zero per stage column
    for (s in 1:S) {
        sigma_ly_raw[s] ~ dnorm(0, sd = 0.5)
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
        log(mu[i]) <- beta_year[year_idx[i]] + beta_stage[stage_idx[i]] +
            b_week[week_idx[i]] + b_ly[ly_idx[i], stage_idx[i]] #stage-indexed
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


# Metropolis comparison (subsample) --------------------------------------------
run_one_chain_rw <- function(seed, glmm_mod, consts, data, monitors, dims) {
    library(nimble)
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
    m <- nimbleModel(glmm_mod, constants = consts, data = data,
                     inits = make_inits(), calculate = FALSE)
    cm <- compileNimble(m)
    conf <- configureMCMC(m, monitors = monitors) # default RW samplers
    mcmc <- buildMCMC(conf)
    cmcmc <- compileNimble(mcmc, project = m)
    t0 <- Sys.time()
    out <- runMCMC(cmcmc, niter = 10000, nburnin = 5000, setSeed = seed,
                   samplesAsCodaMCMC = TRUE)
    attr(out, "elapsed") <- Sys.time() - t0
    return(out)
}

cl <- parallel::makeCluster(4)
samples_rw_list <- parallel::parLapply(
    cl, 1:4, run_one_chain_rw,
    glmm_mod = glmm_mod, consts = sub$consts, data = sub$data_list,
    monitors = monitors, dims = sub$dims
)
parallel::stopCluster(cl)

samples_rw <- coda::as.mcmc.list(samples_rw_list)

sapply(samples_rw_list, \(x) as.numeric(attr(x, "elapsed"), units = "mins"))
coda::gelman.diag(samples_rw, multivariate = FALSE)
coda::effectiveSize(samples_rw)
summary(samples_rw[, "r"])