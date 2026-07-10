#' DESCRIPTION: here I'm going to write out the relatively simple GLMM that 
#' we've used before for these data so I can fit the model 

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)
library(ggplot2)

collated_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/lice-counts-for-regression.csv")
) |> 
    dplyr::filter(month %in% c(3,4))

table(collated_df$week)
table(collated_df$location)

# MODEL CODE

glmm_mod <- nimble::nimbleCode({
    # priors for the fixed effects (just year)
    beta_0 ~ dnorm(0, sd = 10) # the intercept 
    beta_1 ~ dnorm(0, sd = 10) # this is the year effect 

    # prior for the overdispersion 
    alpha ~ dunif(0, 100) # prior for size!

    # prior for random effects (should be priors on the hypermean and the
    # hyper-precision)
    sigma_week ~ dhalfnorm(scale = 2) # trying to shrink week 9 (3 obs)
    sigma_location ~ dhalfnorm(scale = 1) # regularizing because only 3 groups

    # loop for the random effects
    for(i in 1:W) {
        b_week[i] ~ dnorm(0, sd = sigma_week)
    }
    for(j in 1:S) {
        b_location[i] ~ dnorm(0, sd = sigma_location)
    }

    # the likelihood 
    for (i in 1:N) {
        # get the linear predictor on the log scale 
        log(mu[i]) <- beta_0 + inprod(beta_1, X[i]) + gamma[]
    }

})

# the data: 
# N (number of observations I think), W (number of weeks), S (number of sites), 
#