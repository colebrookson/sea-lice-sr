# Analysis decision log — sea-lice species imputation

Running record of choices made during implementation of the Scenario 1–5
imputation pipeline. Two purposes: (1) things that need to be stated or
defended in the methods, (2) deliberately deferred work.

This document tracks *where each decision currently stands*, not a full history
of every change. Superseded approaches are folded into the current statement
rather than kept as archaeology. Update in-session when a decision is flagged.

---

## A. Decisions to reflect in the methods

### A1. `unid_pa` is context-dependent, not uniformly L. salmonis
The raw `unid_PA` column means different things by year (per SCFS metadata):
- **2001**: pre-adult of *unknown species* → enters the imputation pool
  alongside `unid_adult`.
- **2002+**: *L. salmonis* pre-adult of unknown *sex* → counts as a speciated
  Lep motile.
Rationale: in 2001 nothing was speciated, so a Lep-specific pre-adult column
could not have been populated; and Caligus pre-adult motiles were never
separated from adults (they go to `cal_mot`). The audit confirmed `unid_pa` is
populated in exactly 2001 and 2004–2007 — the complement of the years
`lep_pamale`/`lep_pafemale` are populated.
Impact: previously every 2001 pre-adult was silently assigned to L. salmonis,
inflating both 2001 Lep abundance and the observed motile proportion feeding
the model.

### A2. NA handling is protocol-driven, not data-inferred
The raw data cannot distinguish a true zero from a not-recorded blank (stated
explicitly in the SCFS metadata, Class IV.A.6). Therefore blanks are resolved
by a hard-coded year-level protocol table (`lice_protocol()`), not inferred
from the data row by row. Within a stage that *was* counted that year, a blank
is a true zero and is filled; within a stage that was *not* counted (2001
copepodites), the whole stage stays NA and its totals propagate NA rather than
becoming 0.

### A3. Empirical proportions are POOLED, not mean-of-ratios
"Proportion of L. salmonis among the speciated motiles" is computed as
`sum(lep) / sum(speciated)` across all fish in the year — not
`mean(lep_i / speciated_i)`. Mean-of-ratios weights a 1-louse fish equally with
a 40-louse fish and, because the Lep proportion rises with total lice, is
biased downward. Controlled by `POOLED <- TRUE` in `02`; flip to reproduce the
old behaviour.

### A4. Model predictor is TOTAL lice (`all_lice`), not stage-specific total
Both the Scenario 1 GLMs and (forthcoming) Scenario 2 use total lice on the
individual fish as the predictor, per the methods text ("using the number of
total lice on that individual fish"). This makes Scenario 1 vs. Scenario 2 a
clean single-axis comparison (year-level averaging vs. individual-level
prediction), using the *same* fitted models — the only interpretable way to
test the individual-level assumption Scenario 2 exists to test.
- Fitting population unchanged: still fish with ≥1 louse *of that stage* in a
  speciated year. Only the x-axis changed.
- Known wart (shared with Peacock 2013 / Bateman 2016): the response
  denominator is a component of the predictor. Not something these scenarios
  aim to fix.

### A5. Year-level proportion averaged over louse-carrying fish only
Model-derived year scalars (2001 motiles; 2002–2004 copes) average predicted
proportions over fish with ≥1 louse *of that stage*, i.e. the same population
the model was fit on and the only fish the proportion is applied to. Averaging
over zero-louse fish would extrapolate outside the fitted region and drag the
mean toward the intercept.

### A6. Bernoulli draws implemented as Binomial
For each fish, n unidentified lice each get an independent Bernoulli(p) draw.
Since a sum of n iid Bernoulli(p) is Binomial(n, p), this is implemented as
`rbinom(size = n, prob = p)`. Statistically identical to the per-louse loop;
faster and NA-safe. Assumes lice within a fish are exchangeable (all share the
year/fish p) and independent — true under the stated model.

### A7. Ribbon CIs on the link scale
Prediction intervals for the supplementary figures are computed on the link
scale and back-transformed, so they cannot exit [0, 1]. (Response-scale
`fit ± 1.96·se`, as in the old code, can produce ribbons above 1.)

### A9. Louse conservation is verified per-stage through the imputation
`03` includes a check that, for every fish, `lep + cal == all` within each
stage (motile, copepodite, chalimus). NA-on-both-sides is treated as a pass,
which occurs only for 2001 copepodites (the one stage legitimately not counted).
A strict variant — every stage total that *exists* must balance — also passes
with zero exceptions. This is a stateable methods result: no lice are created
or lost during species imputation. The check is deliberately built on plain
`rowSums`/`near`, NOT on an NA-tolerant sum helper: a lice-losing bug (a flipped
`> 0` to `== 0` in `draw_leps`) was caught precisely because the strict check
surfaced the resulting NAs rather than silently summing around them.

### A8. Single imputation realization used downstream (for now)
Downstream fitting uses one imputed dataset (rep 1) rather than pooling across
M replicates. See D1 for the deferred multiple-imputation extension.

---

## B. Open items / things to verify

### B1. Reproduce the published 0.639
`m_old` (old predictor, old averaging population) currently yields 0.676 for
2001, not the published 0.639. Configuration should be a faithful reproduction.
Most likely cause: data vintage (database now runs through July 2025, fitting
pool has grown). Confirm rather than assume before finalizing.

### B2. 2001 `unid_cope` / methods discrepancy
SCFS README says 2001 lice are in `unid_cope`; the data has `unid_cope` empty
in 2001 and the methods say 2001 copes were recorded as chalimus. Data and
methods agree with each other, so coded to them — but worth an email to
Peacock to confirm.

### B3. `lep_nongravid` missingness convention
`lep_nongravid` has zero NAs in 2001 (a year with no speciation), unlike its
neighbours, and only starts showing NAs from 2016. Harmless under the
protocol-table approach (2001 speciated-motile columns are ignored), but the
`01` warning will fire if it holds real nonzero values in 2001. Investigate if
warned.

---

## C. Methods-text housekeeping (not analysis)

### C1. Scenario numbering is broken in the current draft
Two sections labelled "Scenario 2"; a §2.2.3 labelled "Scenario 2" that
describes Scenario 3; §2.2.4 refers back to a "Scenario 3" never introduced
under that name; §2.2.2 and §2.2.3 are near-verbatim duplicates. Renumber
before review; align script filenames to scenario numbers.

---

## D. Deferred extensions

### D1. Multiple-imputation pooling (Rubin's rules)
Currently one realization is carried downstream. Full treatment: fit the
downstream model once per replicate (M=100 already generated in
`lice-counts-imputed-replicates.csv`), then pool with Rubin's rules —
Qbar = mean of estimates, T = Ubar + (1+1/M)·B — to propagate species-
assignment uncertainty into the final CIs. Naïvely pooling all M datasets into
one fit is WRONG (discards between-imputation variance, CIs far too narrow).
Report fraction-of-missing-information per coefficient as the payoff
diagnostic. NB: the Ricker fit uses parametric bootstrap rather than closed-
form SEs, so its pooling is nested (bootstrap-within-imputation) and a
different recipe from the GLM case.

### D2. Separate proportions for pre-adult vs. adult unidentified motiles (2001)
2001 lumps `unid_adult` + `unid_pa` into one draw at one `p_mot`, assuming
adults and pre-adults share a species composition. If they differ, use two
draws with two proportions. Two-line change; faithful to the current methods
text as written.

### D3. Sex/gravidity not assigned to imputed lice
Imputed lice get a species but no sex or gravidity (a drawn-Lep goes to
`lep_mot` generically, not `lep_male`/`lep_gravid`). If any downstream analysis
needs gravid-female-L.-salmonis counts specifically, this pipeline does not
produce them for imputed lice.

### D4. Within-fish species correlation (beta-binomial)
Draws assume lice on the same fish are independent. Biologically they share a
local infection pool, so species may cluster within hosts more than Binomial
allows. Honest alternative would be beta-binomial — a genuinely different
model. Current methods specify independent Bernoulli, so implemented as such.

### D5. Unit tests for the imputation helpers
The `01`/`02`/`03` file family attracted repeated transcription slips —
scalar-recycling `my_sum` in a vectorised mutate, `tibble(x, ...)`, `leop`,
`linewidth` inside `aes()`, and a `> 0`→`== 0` flip in `draw_leps` that
silently manufactured NAs. Each was caught only downstream. A small `testthat`
suite on the pure functions would catch this class at save time, e.g.
`draw_leps`: `draw_leps(3, 1) == 3`, `draw_leps(3, 0) == 0`,
`draw_leps(0, 0.5) == 0`, `is.na(draw_leps(NA, 0.5))`, and a stop when
`size > 0 & is.na(prob)`. Cheap insurance for exactly the failure mode seen.

---

## E. Abundance model (NB GLMM for motile L. salmonis)

Decisions from building the NIMBLE negative-binomial GLMM that estimates
expected motile L. salmonis per juvenile fish per year. Target methods text
(colleagues' prior work, two phrasings): fixed effects for **year** and
**louse stage**; random effects for **week-of-year** and **location-year
combination**; fit to copepodid + chalimus + motile stages to strengthen
random-effect estimation, but **report motile only**.

### E1. Model corresponds to glmmTMB nbinom2
Target frequentist analogue:
`glmmTMB(count ~ factor(year) + stage + (1 | week) + (1 | location:year),
family = nbinom2)`. NIMBLE `dnegbin(p, r)` with `p = r/(r + mu)` gives
mean `mu` and variance `mu + mu^2/r`, so the NIMBLE `r` is glmmTMB's `phi`
(the nbinom2 dispersion). Verify the two fits agree on year coefficients and
`r`; `sigma` components will differ because priors do real work (see E5, F6).

### E2. Response is stage-specific counts in LONG format
Fitting all three stages means each fish contributes up to three rows
(`lep_cope`, `lep_chal`, `lep_mot` stacked into one `count` column with a
`stage` factor). The single summed `all_leps` column is NOT the response for
this model — that was an earlier, different (single-count) model. Final long
frame is 168,452 stage-rows (mot 56,236 / chal 56,236 / cope 55,980; 2001 cope
dropped, see E9).

### E3. Stage fixed effect is a borrowing-strength device
Stage is included so week and location-year random effects are estimated from
~3x the data, not because the stage coefficients are of interest. This is
legitimate ONLY under the assumption that week and location-year effects are
**common across stages** (a site-year is high/low for all stages together; a
week is high/low for all stages together); stage shifts only the overall
level. State this assumption explicitly in the methods — it is what makes the
pooling valid. Report motile row only.

### E4. Year as factor (cell means), stage with a reference level
Year is categorical (one coefficient per year), consistent with prior work.
With year-as-cell-means AND stage-as-factor both carrying a full set of
levels, the design matrix is rank-deficient by 1: a constant can shift between
all year coefficients and all stage coefficients with the fitted values
unchanged (the year cell means absorb the intercept, putting the all-ones
vector in their span and colliding with stage). glmmTMB/lme4 silently drop a
level to fix this; NIMBLE does not, so **one reference level is pinned by
hand**. Chosen: `beta_stage[1] <- 0` with **motile as the reference level**,
so `exp(beta_year[k])` reads directly as the expected motile count in year k
with no contrast arithmetic. Confirm with
`qr(model.matrix(~ 0 + factor(year) + stage))$rank == ncol - 1`.

### E5. Identifiability of year vs. random-effect means (the ridge)
Cell-means year is aliased with the mean of each random effect: adding c to
every year coefficient and subtracting c from every RE level leaves the
likelihood unchanged. Severity scales with 1/sqrt(n_groups); with only 3
location groups this was catastrophic (R-hat ~1.5, ESS ~14 under RW-Metropolis).
Two fixes, both live:
- Location-year interaction (75 levels, not 3 locations — see F2) tightens the
  prior on the RE mean and is what the methods specify anyway.
- Sum-to-zero on each random effect (week AND location-year), implemented as the
  redundant/mean-subtraction form (see E13).

### E6. Non-centered parameterization for random effects
`z ~ N(0,1); b <- sigma * z` rather than `b ~ N(0, sigma)`. Removes the sampler
funnel on thinly-observed groups. Applied to both week and location-year.
Tradeoff: non-centering can hurt well-informed groups; check
centered-vs-non-centered ESS/sec if a group has abundant data.

### E7. Sampler: nimbleHMC, not default RW-Metropolis
Default `configureMCMC` assigns univariate adaptive RW samplers, which mix
poorly on the correlated year/RE posterior even after the worst ridge is
removed. Switched to `nimbleHMC` (requires `buildDerivs = TRUE` on the model).
HMC traverses ridges efficiently; expect R-hat ~1.00 and ESS in the hundreds
from ~1000 post-warmup draws/chain, no thinning. HMC fixes *sampling*, not
*identifiability* — the sum-to-zero constraint (E5) is still required. NIMBLE
AD compiles `dnegbin` cleanly (confirmed at build).

### E8. Priors (settled by prior predictive check)
- `beta_year`, `beta_stage`: `N(0, sd = 1)`. Started at sd = 10; the PPC showed
  sd = 10 produced `mu` tails ~1e20 and undercovered the observed zero fraction
  (0.86). sd = 1 brackets the observed zero fraction near mid-band with sane max
  counts. (The dominant driver of the simulated tail was the max over 25
  independent year cells, not `r` or the RE scales.)
- `sigma_week`, `sigma_ly`: half-normal(0, 0.5), implemented as folded normal
  (see E12).
- `r ~ Gamma(1, 0.5)`. Chosen via the variance function (Var = mu + mu^2/r),
  NOT by moment-matching the counts; confirmed sane in the PPC against
  stage-row counts. `Gamma(0.01, 0.01)` rejected (spike at zero; any shape < 1
  has infinite density at 0).
- PPC checks proportion-of-zeros and count quantiles (max) against observed;
  target is coverage of the observed values, not a prior that already matches
  the data moments.

### E9. Copepodid NA structure (resolved on re-imputed data)
`lep_cope` is a derived column (Lep copepodids apportioned from observed
copepodids by an estimated species proportion). On the re-imputed data its NAs
are no longer ambiguous: `03` apportions every counted copepodid via `p_cope`,
so no present-but-unspeciated residue remains. Verified by per-year partition:
`lep_cope` is NA **iff** `all_cope` is NA **iff** `year == 2001` (the one year
copes were recorded as chalimus), with zero rows in the "present but
unapportioned" case across all 25 years. Consequence: the 2001 cope rows are
**dropped** from the long frame (not split, not zero-filled) — a plain
`filter(!is.na(count))` after the pivot removes exactly those 256 rows and
nothing else. (Historical note: on the *old* imputation this bucket held ~4.9k
genuinely-unapportioned rows needing a cal/unid split; the redo dissolved it.)

### E10. Weeks 9, 28, 33 excluded
Excluded before building `week_idx`, matching the published supplementary
methods (three under-represented weeks, <0.1% of samples across all years).
Done as a filter prior to factoring so week levels stay contiguous — no phantom
RE node, no empty slot in the week sum-to-zero constraint (E5).

### E11. Long-format reshape
`collated_df` (rep-1 imputed, 52,616 fish) reshaped to long: `lep_mot` /
`lep_cope` / `lep_chal` pivoted to one `count` column + `stage` factor
(`names_prefix` strips "lep_"). Row-dropping order: filter weeks 9/28/33 FIRST,
pivot, then `filter(!is.na(count))` (drops the 256 2001 cope rows, E9). Stage
factor levels ordered mot/cope/chal so `stage_idx == 1` is the reference (E4).
All grouping factors `droplevels()`'d AFTER row-dropping so no phantom RE node /
empty sum-to-zero slot. Level-map tibbles kept per index (year/week/stage/ly)
to guard off-by-one when mapping posteriors back.

### E12. Half-normal sigma priors as folded normals
NIMBLE's `T(dnorm(0,1),0,)` half-normal is incompatible with `buildDerivs =
TRUE` ("Truncation via 'T' or 'I' is not supported for derivatives").
Implemented as a folded normal instead: `sigma_raw ~ dnorm(0, sd = 0.5);
sigma <- abs(sigma_raw)`. Confirmed AD-safe — compiled clean under `buildDerivs`,
no `dhalfflat()` fallback needed.

### E13. Sum-to-zero as redundant (mean-subtraction), not hard K-1
Implemented as K free `z_raw ~ N(0,1)`, then `b <- sigma * (z_raw -
mean(z_raw))`, not the hard `z[K] <- -sum(z[1:(K-1)])`. The hard form fails
NIMBLE's dimension inference under `buildDerivs` (the deterministic top element
makes the declared extent of `z` ambiguous — `genVarInfo3` "dimensions smaller
than model specification"; passing `dimensions` explicitly is silently
overridden when the inits length conflicts). The redundant form is the
brms/Stan idiom, better-conditioned for HMC, and all K elements are symmetric
N(0,1) before centering (so the hard form's Kth-element variance asymmetry never
arises). Statistical intent (kill the E5 ridge) is unchanged.

---

## F. Abundance-model open items / deferred

### F1. Copepodid retained (RESOLVED)
Per-year summaries on the re-imputed data: `lep_cope` is the sparsest stage but
not a constant-zero wall. `prop_zero` ranges ~0.42 (2002) to ~0.99 (2009) and
covaries across years with chalimus and motile — sparse years are sparse in all
three stages together, dense years dense together. That covariation is the
signature of a stage responding to the same week/location-year structure the
REs capture, so cope rows carry information for the shared random effects rather
than just dragging sigma toward zeros. Borrowing-strength rationale (E3) holds.
Fit all three stages; report motile. (Contrast the failure mode that would have
triggered a drop: `prop_zero` pinned ~0.99–1.00 in every year with flat ~0
means.)

### F2. Location-year structure (RESOLVED)
`ly` = location × year from observed combinations. 3 sites (Burdwood, Glacier,
Wicklow) × 25 years (2001–2025) = 75 levels, grid FULLY populated — every
site-year cell non-empty, so `ly` is a complete factor with no gaps. This is
more than E5's earlier assumed ~45 (data now runs through 2025), which tightens
the RE-mean prior further and helps the identifiability ridge. No singleton
cells: thinnest is Wicklow_2001 at 66 rows (~22 fish × 3 stages). Non-centering
(E6) still useful on thin cells but is not load-bearing for any singleton, since
there are none.

### F4. Parallelize the 4 chains only if needed
Multiple chains (for R-hat) is orthogonal to multiple cores. `runMCMC(...,
nchains = 4)` runs them sequentially on one compiled model — sufficient while
fast. Parallel `parLapply` (PSOCK, not FORK; each worker recompiles the model,
30-60s overhead) earns its keep once HMC runtime bites, since HMC needs a full
gradient over all rows per leapfrog step. Dispersed per-chain inits are
required either way, or R-hat is toothless.

### F5. Year term: factor vs. trend (scientific framing)
Categorical year gives ~25 independent estimates and no trend parameter —
right for "what was burden each year," wrong for "is there a temporal trend."
If the question is trend, a smooth/monotone year term is the correct object,
not a post-hoc line through 25 points. Flagged, not decided.

### F6. glmmTMB cross-check as a bug detector
Fit the glmmTMB analogue (E1) and compare year coefficients and `r`. Close
agreement validates the NIMBLE code; disagreement on `sigma_week` / `sigma_ly`
is expected (priors); disagreement on year effects means a bug and is faster to
find this way than by re-reading model code.