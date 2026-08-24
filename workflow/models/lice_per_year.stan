data {
// list of the constants (number of obs, number of year x stage
// combos, number of year x location combos, number of stages, number of weeks)
int<lower=0> N // number of obs
int<lower=0> N_ys // # of year x stage
int<lower=0> N_ly // location year combos 
int<lower=0> N_s // number of stages 
int<lower=0> N_wk // weeks

// the vectors of data: 
array[N] int<lower=0> y // response data
array[N] int<lower=1, upper=N_ys> ys_idx // gets the index of that 1,...,N_ys 
array[N] int<lower=1, upper=N_ly> ly_idx // gets the index of that 1,...,N_ly
array[N] int<lower=1, upper=N_s> stage_idx // different, just the three stages
array[N] int<lower=0, upper=N_wk> wk_idx // which week 
}

transformed data {
   // the scaled constants for the priors go here 
   real scale_wk = inv_sqrt(1 - inv(N_wk))
   real scale_ly = inv_sqrt(1 - inv(N_ly))
}

parameters {
   vector[N_yw] beta // fixed effect (\beta_{y_i j}) 
   vector<lower=0>[N_s] sigma_w; // std dev (marginal) on week RE (\sigma_j^{\w})
   vector<lower=0>[N_s] sigma_l;// std dev (marginal) on location x year RE (\sigma_j^{\ell})
   array[N_s] sum_to_zero_vector[N_wk] z_wk; // standardized RE for week (z^{w}_{u,j})
   array[N_s] sum_to_zero_vector[N_ly] z_ly; // standardized RE for location-year (z^{\ell}_{v,j})
   real<lower=0> r; // dispersion parameter (r)
}

transformed parameters {
   matrix[N_wk, N_s] b_wk;// actual b random effect for week (b^{w}_{u,j})
   // actual b random effect for for location-year (b^{\ell}_{v,j})

   // somewhere in here the sum to zero gets inforced


}

model {

    // PRIORS 
    beta_yij ~ normal(0, 5)
    z_w ~ normal(0, (1-(1/W)^-(1/2)))
    z_l ~ normal(0, (1-(1/LY)^-(1/2)))
    sigma_w ~ normal(0, 3)
    sigma_l ~ normal(0, 1)
    r ~ gamma(1, 0.5)

    // LINEAR PREDICTOR 
    // for (i in 1:N)
    //    mu[i,j] <- yrs[years[i]] + b[]

    // the likelihood
    y ~ neg_binomial_2_log(eta, r) # eta bc its log scale 
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
