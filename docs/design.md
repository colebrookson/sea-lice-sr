# Model design

For my own sanity. 

## Overall Goal 

Population-level analysis of sea lice on juvenile pink salmon in the Broughton Archipelago. There are three main components of the analysis:

1. A five-scenario imputation pipeline that decides how we fill in the gaps in the data on wild fish sea lice levels 
2. A bayesian NB GLMM that gets the lep abundance timeseries 
3. Ricker model that takes the lep abundance timeseries and estimates mortality. 

The OoO goes: 


raw fish data 
    -> imputation (per scenario S1-S5, with some M number of replicates each)$^1$
        -> lice for the model, lice_for_model
    -> motile only GLMM
        -> W (lice, draw x year matri) which should be a posterior averaged over observed cells
    -> Ricker model [W is the data, then we use a cut hierarichical model$^2$]
        -> c 
    -> mortality = 1 - exp(-c * W)

### GLMM Structure 

Right now, we use only motiles in the GLMM, with two random effects on the intercept, and just the fixed effect of year. I've written some draft methods (some for the SI probably) here: 


In Bateman et al. (2016), there was a second fixed effect for stage, but that requires the two random effects to be common across stages. We found that this assumption was not supported in our data, as allowing year effects to vary by stage improved fit significantly. So, our model, given that, can be written as the following. 

We say that for each fish $i = 1,...,N$, with $N \simeq 56,000$, the response variable, $y_i$ is the motile lep count (dependent on the scenario), 

$$
y_i \sim \text{NegBin}(\mu_i, r).
$$
where $r$ is the negative binomial dispersion parameter, so that $\mathbb{E}[y_i] = \mu_i$ and $\text{Var}(y_i) \mu_i + \frac{\mu_i^2}{r}$. 

The linear predictor is given by a fixed effect for year, treated as a factor with a separate coeffcieint per year, and two random effects, one for week of year and one for location-year ($\text{ly}$) combination (three locations: Burdwood, Glacier, and Wicklow, and 25 years). 

$$
log \mu_i = \beta_{\text{year}[i]} + b_{\text{week[i]}}^{\text{wk}} + b_{\text{ly[i]}}^{\text{ly}}. 
$$ 

Weeks 9, 28, and 33 were excluded due to insufficient numbers of samples. Therefore, for weeks $j=1,...,21$, site-years $l=1,...,75$, and years $k=1,...,25$,

$$
\begin{aligned}
b_j^{\text{wk}} =& \sigma_{\text{wk}}(z_j^{\text{wk}} - \bar{z}^{\text{wk}}) \\
b_l^{\text{ly}} =& \sigma_{\text{ly}}(z_l^{\text{ly}} - \bar{z}^{\text{ly}}) \\
\sigma_{\text{wk}} =& \lvert{\tilde{\sigma}_{\text{wk}}} \\
\sigma_{\text{ly}} =& \lvert{\tilde{\sigma}_{\text{ly}}}
\end{aligned}
$$

We treat the random effects as independent normal random intercepts, constrained to sum to zero such that $\exp(\beta_k)$ are the expected motile count in year $k$ at an average week and average location-year. We also sampled in non-centered form to improve the mixing. Our priors were as follows:

$$
\begin{aligned}
\beta_k \sim& \ \mathcal{N}(0,1) \\
z_j^{\text{wk}} \sim& \ \mathcal{N}(0,1) \\
z_l^{\text{ly}} \sim& \ \mathcal{N}(0,1) \\
\tilde{\sigma_{\text{wk}}} \sim& \ \mathcal{N}(0, 3) \\
\tilde{\sigma_{\text{ly}}} \sim& \ \mathcal{N}(0, 1) \\
r \sim& \ \text{Gamma}(1,0.5)
\end{aligned}
$$






## Notes
1. We need $\mathcal{M}$ replicates because the imputation (at least options 1-4) are all statistical in nature, and so they represent a random draw of a dataset. That's not good, so we can loosen the challenges there by essentially running some $\mathcal{M}$ number of models and using it essentially as an ensemmble of options. We can always do it the older way too where we just plug in a single value, and do that by just taking `colMeans()` of the full draw x year matrix. 
2. I actually don't know if it will be a cut model yet, a cut model essentially feeds the whole posterior of the GLMM to the Ricker model as a vector of data (I think?) but this isn't fully hierarchical. We can test whether or not a fully hierarchical one is better by some test [FILL IN]. 