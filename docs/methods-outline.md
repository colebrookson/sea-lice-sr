# Outline of Proposed Methods

## Our question

### 1) - Is there a relationship between lice on wild fish and lice on farms? 

Need to establish a relationship between a) the number of lice on out-migrating juvenile salmon (DOES THIS NEED TO BE DONE FOR ALL WILD SALMON SPP SEPARATELY? STEPH DID THIS BY LUMPING ALL JUV SALMON IN HER PROC B PAPER) and b) the number of lice on farms. This would need to be done for all years we have both farm data and out migrating lice data. If we can identify a corrlation between the two values higher than some pre-defined threshold (HOW TO CHOOSE THIS? SOME SORT OF R^2 > 0.4?) 

Therefore, we could re-do Marty's PNAS paper analysis looking at the number of lice on farmed fish (THIS WOULD BE WITH BATI DATA?) and then also the amount of lice on out-migrating fish (Steph's Ecol. App. paper) (IS THIS SALMON COAST DATA?)

Then, using the GLMM framework steph has already done in her Proc B paper, we can simply change that to a Bayesian approach, where we have data on the number of lice on fish $i$ in week $j$ of year $k$, in location $q$. So the expected value can be formulated 

$$ \phi(E(Y_{ijkq}|\textbf{X}_{ijkq},b_j,b_q)) \sim \beta_1 \textbf{X}_{ijkq} + (\beta_0 + b_{j,0j} + b_{k,0i})$$


where $\phi$ is the link function, $\beta_1$ is the fixed effect for year and the $b$ values are random intercepts for week ($j$) and location ($q$). Then we define the priors as: 


$$
\pi(\beta_0) = N(0,x_1) \\ 
\pi(\beta_1) = N(0,x_2) \\
\pi(b_j) = N(0,\tau^2) \\
\pi(b_k) = N(0,\tau^2) \\
\pi(\tau^2) = IG(0,0.025) 
$$




## Summary of Steph's Paper (Peacock et al. 2013 - Ecol App)

* Abundance of *Leps* per wild juv. pink salmon per year were calculated from weekly monitoring data using a GLMM 
  * Fixed effects: year 
  * Random effects: sample site + week 
  * Z-I neg binom 
* Relationship between total number of parasitide treatments and proportion of those treatments occurring in winter in the KTF and Broughton -- regression 
* Yearly *leps* estimates were then related to:
  * total lice on farmed salmon
  * management changes 
  * number of winter treatments 
  * proportion of total treatments occuring during winter on farms in Broughton 
* **Survival of pinks related to average number of lice per wild juv. salmon on the premise that that was related to both farm lice and adaptive changes in management -- this provides a covariate that captures actual effect on wild salmon survival**
* Used hierarchical ricker: 
  $$ R_{i,t} = N_{i,t-2} \text{exp} [r - b_iN_{i,t-2} - cW_{a,t-2} + \theta_i + \theta_{a,t} + \epsilon_{i,t}] $$
    * $R_{i,t}$ = recruitment of pop $i$ in year $t$ 
    * $N_{i,t}$ = abundance os spawners in pop $i$ in year $t-2$
      * this is lagged 2 years for pinks cos of life cycle, would be different for other spp 
    * $r$ = growth rate 
    * $b_i$ = density dependence parameter for each pop $i$
    * $W_{a,t-1}$ = average lice per wild juv. salmon in previous year 
      * this comes from the GLMM
    * $c$ is strength of the relationship between pink salmon survival and lice 
      * estimated % mortality of pinks due to sea lice is $1- exp(-cW_{a,t-1})$
    * $\theta_t$ = spatially coherent variation among *all* pop'ns -- normal var, $\mu = 0$, variance is estiamted 
    *  $\theta{a,t}$ = spatially coherent variation among pop'ns w/in a management area 
    *  $\epsilon_{i,t}$ is random anual variation independent among populations
 * Linear version of this model:
  $$ ln\left(\frac{R_{i,t}}{N_{i,t-2}}\right) = r - b_iN_{i,t-2} - cW_{a,t-2} + \theta_i + \theta_{a,t} + \epsilon_{i,t}$$