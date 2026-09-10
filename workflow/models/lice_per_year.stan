data {
// list of the constants (number of obs, number of year x stage
// combos, number of year x location combos, number of stages, number of weeks)
int<lower=0> N; // number of obs
int<lower=0> N_ys; // # of year x stage
int<lower=0> N_ly; // location year combos 
int<lower=0> N_s; // number of stages 
int<lower=0> N_wk; // weeks

// the vectors of data: 
array[N] int<lower=0> y;// response data
array[N] int<lower=1, upper=N_ys> ys_idx; // gets the index of that 1,...,N_ys 
array[N] int<lower=1, upper=N_ly> ly_idx; // gets the index of that 1,...,N_ly
array[N] int<lower=1, upper=N_s> stage_idx; // different, just the three stages
array[N] int<lower=1, upper=N_wk> wk_idx; // which week 
}

transformed data {
   // the scaled constants for the priors go here 
   real scale_wk = inv_sqrt(1 - inv(N_wk));
   real scale_ly = inv_sqrt(1 - inv(N_ly));
}

parameters {
   vector[N_ys] beta; // fixed effect (\beta_{y_i j}) 
   vector<lower=0>[N_s] sigma_wk; // std dev (marginal) on week RE (\sigma_j^{\w})
   vector<lower=0>[N_s] sigma_ly;// std dev (marginal) on location x year RE (\sigma_j^{\ell})
   array[N_s] sum_to_zero_vector[N_wk] z_wk; // standardized RE for week (z^{w}_{u,j})
   array[N_s] sum_to_zero_vector[N_ly] z_ly; // standardized RE for location-year (z^{\ell}_{v,j})
   real<lower=0> r; // dispersion parameter (r)
}

transformed parameters {
   matrix[N_wk, N_s] b_wk; // actual b random effect for week (b^{w}_{u,j})
   matrix[N_ly, N_s] b_ly; // actual b random effect for for location-year (b^{\ell}_{v,j})

   for(s in 1:N_s) {
        b_wk[, s] = sigma_wk[s] * z_wk[s];
        b_ly[, s] = sigma_ly[s] * z_ly[s];
   }
}

model {
    vector[N] eta;
    // PRIORS 
    beta ~ normal(0, 5);
    // these need a loop since they're multi-dimensional
    for (s in 1:N_s) {
        z_wk[s] ~ normal(0, scale_wk);
        z_ly[s] ~ normal(0, scale_ly);
    }

    sigma_wk ~ normal(0, 3);
    sigma_ly ~ normal(0, 1);
    r ~ gamma(1, 0.5);

    // LINEAR PREDICTOR 

    for(n in 1:N) {
        eta[n] = beta[ys_idx[n]] + 
                    b_wk[wk_idx[n], stage_idx[n]] + 
                    b_ly[ly_idx[n], stage_idx[n]];
    }

    // the likelihood
    y ~ neg_binomial_2_log(eta, r); // eta bc its log scale 
}
