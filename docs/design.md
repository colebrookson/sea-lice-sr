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

Right now, we use a similar kind of model that Andrew does in the 2016 paper. I've written some draft methods (some for the SI probably) here: 

We say that for each fish $f = 1,...,F$, the response variable, $y_{ijf}$ is the count of *L. salmonis* (dependent on the scenario), in stage $j$, sampled at site $i$, where $\ell_i$ and $y_i$ denote the location and year of that site, and $w_i$ its week of year. Therefore, we say that

$$
y_{ijf} \sim \text{NegBin}(\mu_{ij}, r).
$$

where $r$ is the negative binomial dispersion parameter, so that $\mathbb{E}[y_{ij}] = \mu_{ij}$ and $\text{Var}(y_{ij}) = \mu_{ij} + \frac{\mu_{ij}^2}{r}$.


The linear predictor is given by an interaction fixed effect for year (treated as a factor with a separate coefficient per year–stage combination) and louse stage, and two random effects, one for week of year, and one for location-year combination of three locations (Burdwood, Glacier, and Wicklow), and 25 years.

$$
\log \ \mu_{ij} = \beta_{y_i j} + b_{w_i, j}^{w} + b_{y_i/\ell_i, j}^{\ell}
$$

Both random effects are stage-specific, with variances estimated independently for each stage, and each is constrained to sum to zero within each stage [CITE - probably Gelman & Hill book]. Writing $u = w_i$ and $v = y_i/\ell_i$ for the week and location-year of site $i$, and with weeks $u = 1,\dots,21$ (weeks 9, 28, and 33 were excluded due to insufficient numbers of samples), location-years $v = 1,\dots,75$, and stages $j \in \{\text{copepodid}, \text{chalimus}, \text{motile}\}$,

$$
b^{w}_{u,j} \sim \mathcal{N}\!\left(0, \sigma^{w\,2}_{j}\right), \qquad \sum_{u=1}^{21} b^{w}_{u,j} = 0 \quad \text{for each } j
$$

$$
b^{\ell}_{v,j} \sim \mathcal{N}\!\left(0, \sigma^{\ell\,2}_{j}\right), \qquad \sum_{v=1}^{75} b^{\ell}_{v,j} = 0 \quad \text{for each } j
$$

independently across levels and across stages. The diagonal structure allows the magnitude of week-to-week and location-year variation to differ among louse stages, while treating the three stages' effects at a given week or location-year as uncorrelated [Bates et al. (parsimonious models one)]. An unstructured covariance, which additionally estimates the three pairwise correlations, gave no improvement in fit (see Fig [TODO - fig!]).

## Parameterization and priors

We sampled the random effects in non-centred form [Betancourt & Girolami (2015)], expressing each as the product of a stage-specific standard deviation, and a standardized random effect:

$$
b^{w}_{u,j} = \sigma^{w}_{j} \, z^{w}_{u,j}, \qquad
b^{\ell}_{v,j} = \sigma^{\ell}_{j} \, z^{\ell}_{v,j}
$$

with the sum-to-zero constraint imposed on within each stage, and the prior on the constrained vector is scaled by $(1 - 1/n)^{-1/2}$, so that $\sigma^{w}_{j}$ and $\sigma^{\ell}_{j}$ are marginal standard deviations of the corresponding effects [Stan user guide] for stage $j$. Our priors were as follows:

$$
\begin{aligned}
\beta_{y_i j} &\sim \mathcal{N}(0, 5) \\
z^{w}_{\cdot,j} &\sim \mathcal{N}\!\left(0, \left(1 - \tfrac{1}{21}\right)^{-1/2}\right) \\
z^{\ell}_{\cdot,j} &\sim \mathcal{N}\!\left(0, \left(1 - \tfrac{1}{75}\right)^{-1/2}\right) \\
\sigma^{w}_{j} &\sim \mathcal{N}^{+}(0, 3) \\
\sigma^{\ell}_{j} &\sim \mathcal{N}^{+}(0, 1) \\
r &\sim \text{Gamma}(1, 0.5)
\end{aligned}
$$

where $\mathcal{N}^{+}$ denotes a half-normal distribution and the fixed effects comprise 74 year–stage cells. These priors are weakly informative on the scale of the data. [KEEP THIS?? They differ from the priors used by Bateman et al. (2016), who followed the then-standard practice of specifying diffuse normal priors via a small precision and inverse-gamma priors on the variance components; such priors are now known to be sensitive to their tuning constants for variance parameters, and we place priors on the standard deviation scale instead.]

## Interpretation of the coefficients

Because the random effects are constrained to sum to zero within each stage, we present *only* the marginal expected abundance across all weeks and location-years, computed as:

$$
W_{y_i j} = \mathbb{E}\!\left[\exp\!\left(\beta_{y_i j} + b^{w} + b^{\ell}\right)\right] = \exp\!\left(\beta_{y_i j} + \tfrac{1}{2}\left(\sigma^{w\,2}_{j} + \sigma^{\ell\,2}_{j}\right)\right)
$$

$W$ is the quantity carried forward into the spawner–recruit model, and we computed it within each posterior draw so that uncertainty in the variance components propagates. 

## The 2001 sampling protocol

Louse identification protocols changed over the study period. In 2001, copepodid-stage lice were recorded as chalimus rather than being distinguished as a separate stage, so no copepodid counts exist for that year. The year–stage grid accordingly contains 74 cells rather than 75, and no copepodid abundance is estimated for 2001.

This has three consequences for interpretation. First, the total *L. salmonis* abundance per fish in 2001 is the sum of the motile and chalimus estimates, and is complete, since the copepodid-stage lice are contained within the chalimus counts. Second, the 2001 chalimus estimate is not comparable with chalimus estimates in other years: it includes copepodid-stage lice, and its species allocation is derived from the motile proportion alone, rather than from the average of the copepodid and motile proportions used elsewhere. Third, no 2001 motile *L. salmonis* were identified to species directly, so the 2001 motile estimate depends on the imputed species proportion for that year. 

## Notes
1. We need $\mathcal{M}$ replicates because the imputation (at least options 1-4) are all statistical in nature, and so they represent a random draw of a dataset. That's not good, so we can loosen the challenges there by essentially running some $\mathcal{M}$ number of models and using it essentially as an ensemmble of options. We can always do it the older way too where we just plug in a single value, and do that by just taking `colMeans()` of the full draw x year matrix. 
2. I actually don't know if it will be a cut model yet, a cut model essentially feeds the whole posterior of the GLMM to the Ricker model as a vector of data (I think?) but this isn't fully hierarchical. We can test whether or not a fully hierarchical one is better by some test [FILL IN]. 