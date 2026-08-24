data {
// list of the constants (number of obs, number of fish, number of year x stage
// combos, number of year x location combos, number of stages, number of weeks)

// the vectors of data: 
    // year 
    // year x stage beta_yij
    // year x location (v = y_i / \ell_i)
    // counts 
    // stage
    // week of year (u = w_i)

}

parameters {
   // dispersion parameter (r)
   // fixed effect (\beta_{y_i j}) 
   real<lower=0> sigma_w; // std dev (marginal) on week RE (\sigma_j^{\w})
   real<lower=0> sigma_l;// std dev (marginal) on location x year RE (\sigma_j^{\ell})
   // standardized RE for week (z^{w}_{u,j})
   // standardized RE for location-year (z^{\ell}_{v,j})
}

transformed parameters {
   // linear predictor [N] (\mu_{ij})
   // actual b random effect for week (b^{w}_{u,j})
   // actual b random effect for for location-year (b^{\ell}_{v,j})

   // somewhere in here the sum to zero gets inforced

   // for (i in 1:N)
   //    mu[i,j] <- yrs[years[i]] + b[]
}

model {

    // PRIORS 
    beta_yij ~ normal(0, 5)
    z_w ~ normal(0, (1-(1/W)^-(1/2)))
    z_l ~ normal(0, (1-(1/LY)^-(1/2)))
    sigma_w ~ normal(0, 3)
    sigma_l ~ normal(0, 1)
    r ~ gamma(1, 0.5)
    
    // the likelihood
    y ~ neg_binomial_2(mu, r)
}








data {
    int <lower=1> N; // number of total observations (?????)
    int <lower=1> F; // number of fish 
    int <lower=1> Y; // number of years (i.e. number of FE levels)
    int <lower=1> N_ly; // number of loc x year combos 
    int <lower=1> J; // number of stages
    int <lower=1> L; // number of locations
    int <lower=1> W; // number of weeks
    int <lower=1> N_sy; // number of stage-year combos 


    int <lower=1, upper=F> fish[N]
    int <lower=1, upper=N_years> years[N]; // vector of year values (?)
    int <lower=1, upper=N_locs> l_y[N]; // vector of location year combos
    vector[N] y; // response variable [LICE PER FISH]
}

parameters {
   vector[N_locs] eta; // linear predictor
   vector[N_years] yrs; // ?
   real <lower=0> sigma_locs; // the sigma value for the random effect
   real <lower=0> sigma_epsilon; // noise term 
}

transformed parameters {
   vector[N_locs] locs; // vector ? 
   vector[N] yhat; // estimations of y 

   locs <- sigma_locs * eta;

   for (i in 1:N)
    mu[i] <- yrs[years[i]] + locs[locations[i]]; // this is getting yhat
}

model {
    y ~ neg_binomial_2(mu, sigma_epsilon)
}
