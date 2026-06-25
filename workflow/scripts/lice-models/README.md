# *L. salmonis* Abundance Estimation
## Overview

This analysis estimates *Lepeophtheirus salmonis* abundance across different life stages (copepodites, motiles, and chalimus) for years with incomplete species identification data. The approach uses empirical proportions and logistic regression to impute missing species-level counts.

## Methodology

### Part 1: Copepodites (2005 - Present)

For years where copepodites were mostly speciated (2005 onwards), we estimate the number of unidentified copepodites that were actually *L. salmonis* using the **empirical proportion** of *L. salmonis* among the speciated copepodites.

### Part 2: Motiles (2002 - Present)

For years where motiles were mostly speciated (2002 onwards), we estimate the number of unidentified motiles that were actually *L. salmonis* using the **empirical proportion** of *L. salmonis* among the speciated motiles. Note that we assume that all pre-adults are in fact *L. salmonis.*

### Part 3: Model-Based Prediction (2001-2004)

For years where copepodites (2002-2004) and motiles (2001) were counted but **never speciated**, we estimate the number of *L. salmonis* in these life stages using **predicted proportions** from logistic regressions fit to the speciated years' data:
- Copepodite model: trained on 2005+ data
- Motile model: trained on 2002+ data

### Part 4: Individual-Level Imputation

We calculate the mean proportion of *L. salmonis* in a given year/stage and apply it to individual observations via **random draws from a Bernoulli distribution**. 

For each unidentified louse:
1. Draw from Bernoulli(p), where p = predicted proportion of *L. salmonis*
2. If draw = 1, count that louse as *L. salmonis*
3. Repeat for each unidentified louse

This approach is more biologically realistic than assigning fractional louse counts to individual fish.

### Part 5: Chalimus Stage Estimation

We estimate *L. salmonis* chalimus-stage lice by applying the **average** of the *L. salmonis* proportions for copepodites and motiles.

**Special case (2001)**: Since copepodites were counted as chalimus in 2001, we estimate the chalimus *L. salmonis* proportion using only the motile *L. salmonis* proportion.

## Data Structure

The analysis expects the following columns:
- `year`: Sampling year
- `lep_cop`: Count of identified *L. salmonis* copepodites
- `all_cop`: Total count of copepodites
- `lep_mot`: Count of identified *L. salmonis* motiles  
- `all_mot`: Total count of motiles
- Life stage indicators (copepodite, motile, chalimus)