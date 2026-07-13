#' DESCRIPTION: here I'm going to write out the relatively simple GLMM that 
#' we've used before for these data so I can fit the model 

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)
library(nimble)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
) |> 
    dplyr::filter(month %in% c(4, 5, 6))

table(collated_df$week)
table(collated_df$location)

# super quick PPC on the gamma
# r_draws <- rgamma(n = 2000, shape = 1, rate = 0.5)
# sim <- rnbinom(n = 2000, size = r_draws, mu = mean(collated_df$all_leps))

# quantile(sim, c(0.5, 0.9, 0.99, 1))
# quantile(collated_df$all_leps, c(0.5, 0.9, 0.99, 1))

# make the model itself
glmm_mod <- nimble::nimbleCode({
    # year as cell means, no intercept ----------------------------------------
    for (k in 1:Yr) {
        beta_year[k] ~ dnorm(0, sd = 10)
    }

    # prior for the overdispersion ---------------------------------------------
    #' I picked this by looking at mean(collated_df$all_leps) = ~0.83 
    #' because here, the shape and rate a and b have mean a/b and variance 
    #' a/b^2. So then var(collated_df$all_leps) = ~4.79. So given that, 
    #' Gamma with mean 2, sd 2 gives shape = 1, rate = 0.5.
    r ~ dgamma(shape = 1, rate = 0.5) # must be > 0

    # prior for random effects -------------------------------------------------
    sigma_week ~ dunif(0, 5) # trying to shrink week 9 (3 obs)
    sigma_location ~ dunif(0, 2) # regularizing because only 3 groups

    # loop for the random effects
    for (i in 1:W) { 
        # this is the non-centered parameterization which we need because
        # some groups are really thinly observed
        z_week[i] ~ dnorm(0, sd = 1)
        b_week[i] <- sigma_week * z_week[i]
    }
    for(j in 1:L) {
        b_location[j] ~ dnorm(0, sd = sigma_location)
    }

    # the likelihood -----------------------------------------------------------
    for (i in 1:N) {
        # get the linear predictor on the log scale 
        #' NOTE: we don't want a beta_0 here because that gives each year it's 
        #' own free coefficient. So, it's just indexed directly like this: 
        #'              beta_1[year_idx[i]]
        #' The other option is year as a random effect, where 
        #' beta_year[k] ~ dnorm(mu_year, sd = sigma_year), which partially pools
        #' each year toward each other, which gives a variance component for 
        #' interannual variability. 

        log(mu[i]) <- beta_year[year_idx[i]] + 
                          b_week[week_idx[i]] + b_location[location_idx[i]]

        # convert expected count (mu) to probability parameter
        p[i] <- r / (r + mu[i])
        
        # now feed this into the actually poisson bit
        Y[i] ~ dnegbin(p[i], r)
    }
})

# the data: 
# N (number of observations I think), W (number of weeks), L (number of 
# locations), y (response)
#

# define the constants 
constants <- list(
    N = nrow(collated_df), 
    W = length(unique(collated_df$week)),
    L = length(unique(collated_df$location)),
    Yr = length(unique(collated_df$year)),
    week_idx = as.numeric(as.factor(collated_df$week)),
    location_idx = as.numeric(as.factor(collated_df$location)), 
    year_idx = as.numeric(as.factor(collated_df$year))
)

# define the data 
data_list <- list(
    Y = collated_df$all_leps
)

# define initial values 
inits <- list(
    beta_year = rep(0, constants$Yr),
    r = 1,
    sigma_location = 1, 
    sigma_week = 1, 
    z_week = rnorm(n = constants$W, mean = 0, sd = 0.1), # instead of b_week
    b_location = rnorm(n = constants$L, mean = 0, sd = 0.1)
)

nb_model <- nimble::nimbleModel(
    code = glmm_mod, 
    constants = constants, 
    inits = inits, 
    data = data_list
)
C_nb_model <- nimble::compileNimble(nb_model)

# the MCMC gets configured here
nb_mcmc_config <- nimble::configureMCMC(nb_model, 
    monitors = c(
        "beta_year", "b_week", "b_location",
        "sigma_week", "sigma_location", "r"
    ) 
)

# time to compile!
nb_mcmc <- nimble::buildMCMC(nb_mcmc_config)
C_nb_mcmc <- nimble::compileNimble(nb_mcmc, project = nb_model)

# run and sample this thang
samples <- nimble::runMCMC(C_nb_mcmc, niter = 10000, nburnin = 2000, thin = 5)
