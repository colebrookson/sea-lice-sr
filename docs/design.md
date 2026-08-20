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

We are currently using only the motiles in the GLMM. 

## Notes
1. We need $\mathcal{M}$ replicates because the imputation (at least options 1-4) are all statistical in nature, and so they represent a random draw of a dataset. That's not good, so we can loosen the challenges there by essentially running some $\mathcal{M}$ number of models and using it essentially as an ensemmble of options. We can always do it the older way too where we just plug in a single value, and do that by just taking `colMeans()` of the full draw x year matrix. 
2. I actually don't know if it will be a cut model yet, a cut model essentially feeds the whole posterior of the GLMM to the Ricker model as a vector of data (I think?) but this isn't fully hierarchical. We can test whether or not a fully hierarchical one is better by some test [FILL IN]. 