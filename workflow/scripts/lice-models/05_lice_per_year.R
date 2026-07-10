#' DESCRIPTION: here I'm going to write out the relatively simple GLMM that 
#' we've used before for these data so I can fit the model 

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
) |> 
    dplyr::filter(month %in% c(4, 5, 6))

table(collated_df$week)
table(collated_df$location)

# make the model itself
glmm_mod <- nimble::nimbleCode({
    # priors for the fixed effects (just year) ---------------------------------
    beta_0 ~ dnorm(0, sd = 10) # the intercept 
    beta_1 ~ dnorm(0, sd = 10) # this is the year effect 

    # prior for the overdispersion ---------------------------------------------
    r ~ dgamma(shape = 0.01, rate = 0.01) # must be > 0

    # prior for random effects -------------------------------------------------
    sigma_week ~ dunif(0, 5) # trying to shrink week 9 (3 obs)
    sigma_location ~ dunif(0, 2) # regularizing because only 3 groups

    # loop for the random effects
    for(i in 1:W) {
        b_week[i] ~ dnorm(0, sd = sigma_week)
    }
    for(j in 1:L) {
        b_location[i] ~ dnorm(0, sd = sigma_location)
    }

    # the likelihood -----------------------------------------------------------
    for (i in 1:N) {
        # get the linear predictor on the log scale 
        log(mu[i]) <- beta_0 + inprod(beta_1, X[i, P]) +
                          b_week[week_idx[i]] + b_location[location_idx[i]]

        # convert expected count (mu) to probability parameter
        p[i] <- r / (r + mu[i])
        
        # now feed this into the actually poisson bit
        y[k] ~ dnegbin(p[i], r)
    }
})

# the data: 
# N (number of observations I think), W (number of weeks), L (number of 
# locations), y (response)
#

# define the constants 
constants <- list(
    N = nrow(collated_df), 
    P = ncol(collated_df[, "year"]), # just one here
    W = length(unique(collated_df$week)),
    L = length(unique(collated_df$location)),
    week_idx = as.numeric(as.factor(collated_df$week)),
    location_idx = as.numeric(as.factor(collated_df$location))
)

# define the data 
data_list <- list(
    Y = collated_df$all_leps
)

# define initial values 
inits <- list(
    beta_0 = 0, 
    beta_1 = 0, 
    r = 10, 
    sigma_location = 1, 
    sigma_week = 1, 
    b_week = rnorm(0, 0.1), 
    b_location = rnorm(0, 0.1)
)

nb_model <- nimble::nimbleModel(
    code = glmm_mod, 
    constants = constants, 
    inits = inits, 
    data = data_list
)

# the MCMC gets configured here
nb_mcmc_config <- nimble::configureMCMC(nb_model, 
    monitors = c(
        "beta_0", "beta_1", "b_week", "b_location", 
        "sigma_week", "sigma_location"
    ) 
)

# time to compile!
nb_mcmc <- nimble::buildMCMC(nb_mcmc_config)
C_nb_model <- nimble::compileNimble(nb_model)
C_nm_mcmc <- nimble::compileNimble(nb_mcmc)

# run and sample this thang
samples <- nimble::runMCMC(C_nb_mcmc, niter = 10000, nburnin = 2000, thin = 5)
