## 20 August 2026

The last couple of days have been a big restructure on the whole repo and I'm now finally getting back to fitting the yearly lice models. I'll put some notes here on what I've tried today and and what worked. 

* Tried a bunch of different frequentist options (in the `different_stage_comparisons.R` file), but figured that a normal set of i.i.d. random intercepts and one fixed effect seemed to work well 
* Next steop is to fix the NIMBLE code, that needs to run again as it's own standalone thing before it gets incorporated into the Ricker model 
* I wrote some prose for the methods and that's in the `design.md` doc. 

## 21 August 2026

Decided I'm going to move this from NIMBLE over to Stan. I don't see any utility in staying in NIMBLE if I'm not using it's normal MCMC, I'd rather use the HMC to get some speed-up in the lice-per-year model. 