# General Notes & Thoughts on Fitting Bayesian GLM for the Relationship Between Lice on Juvenile Wild Salmon and on Farmed Salmon 

### AUTHOR: Cole B. Brookson
### DATE INSTANTIATED: 2022-02-29

## General

Following the four steps to analysis, we need to: 

1. Specify a joint distribution for the outcomes and all unkowns - this is proportional to a posterior distribution of the unknowns conditional on the observed data - [info on how we used `rstan` for this](https://mc-stan.org/rstanarm/articles/count.html) 
2. Draw from the posterior distribution 
3. Evaluate how well the model fits
4. Draw from the posterior predictive distribution of the outcomes and visualize how a manipulation of the predictor affects a function of the outcome 

