# motile-model-predictions

- saved: 2026-09-10 03:26:05
- git: not a git repo
- size: 8 x 6 in @ 300 dpi

## Notes

Proportion of motile sea lice that were L. salmonis as a function of the total number of lice (all stages, all species) on an individual fish. Points are the observed proportion among speciated motiles for each fish carrying at least one motile louse in a year in which motiles were speciated (2002-present), not to worry,  jittered horizontally only. Line is a binomial GLM with a logit link, Lep motiles as successes and Caligus motiles as failures, so each fish is weighted by the number of motile lice it carries. This model supplies the L. salmonis
     proportion for 2001, the only year in which motiles were counted but never speciated; the mean predicted proportion across 2001 fish carrying motiles is 0.804. Fitted support spans 1-80 total lice; 0.0% of 2001 fish fall outside it.
