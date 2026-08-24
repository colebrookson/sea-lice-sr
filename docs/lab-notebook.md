## 20 August 2026

The last couple of days have been a big restructure on the whole repo and I'm now finally getting back to fitting the yearly lice models. I'll put some notes here on what I've tried today and and what worked. 

* Tried a bunch of different frequentist options (in the `different_stage_comparisons.R` file), but figured that a normal set of i.i.d. random intercepts and one fixed effect seemed to work well 
* Next steop is to fix the NIMBLE code, that needs to run again as it's own standalone thing before it gets incorporated into the Ricker model 
* I wrote some prose for the methods and that's in the `design.md` doc. 

## 21 August 2026

Decided I'm going to move this from NIMBLE over to Stan. I don't see any utility in staying in NIMBLE if I'm not using it's normal MCMC, I'd rather use the HMC to get some speed-up in the lice-per-year model. 

Made some decisions: 
* there's now a file with all the frequentist options that compare everything in one place. Having seen all the results, I'm sticking with the motile-only option, though I'm tempted to have a SI bit that shows what the results would look like if I did the full three-stage model and just predicted out the 

## 24 August 2026

More modeling decisions! I think I'm going to move forward with the same three-stage strucutre (for fixed effects) that andrew used. I'm still going to keep the random effects structure as I personally think there's no real contest, but since the fixed effects structure doesn't give us much improvement I want to get rid of as much reviewer push back as possible. Will re-write the `design.md` doc today 