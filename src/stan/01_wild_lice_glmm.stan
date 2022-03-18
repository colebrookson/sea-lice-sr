data {
    int <lower=1> N; // number of observations 
    int <lower=1> N_years; // number of years (i.e. number of fixed effect levels)
    int <lower=1> N_locs; // number of locations (i.e. number of randome effect levels)
    int <lower=1, upper=N_years> years[N]; // vector of year values (?)
    int <lower=1, upper=N_locs> locations[N]; // vector of locations (?)
    vector[N] y; // response variable [LICE PER FISH]
}

parameters {
   vector[N_locs] eta; // ? 
   vector[N_years] yrs; // ?
   real <lower=0> sigma_locs; // the sigma value for the random effect
   real <lower=0> sigma_epsilon; // noise term 
}

transformed parameters {
   vector[N_locs] locs; // vector ? 
   vector[N] yhat; // estimations of y 

   locs <- sigma_locs * eta;

   for (i in 1:N)
    yhat[i] <- yrs[years[i]] + locs[locations[i]]; // this is getting yhat
}

model {
    eta ~ normal(0, 1); // prior

    y ~ neg_binomial_2(yhat, sigma_epsilon)
}
