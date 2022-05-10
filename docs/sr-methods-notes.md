---
title: Bayesian Stock-Recruit Methods
author: Cole B. Brookson
date: 2022-04-15
---

## Hierarchical Ricker Model 

* Spawner-recruit data 
  * spatially structured, by populations exposed to salmon farms (Broughton Archipelago) and reference regions (Areas 7-11) 
* Effects
  * year & management area w/in year as random & nested random effects respectively 
  * density-dependent morality was treated as a fixed factor per population (i.e. different for even- and odd-year populations within the same river)
  * growth rate is fixed factor
  * average lice per wild juvenile salmon (estimated from the GLMM) was included as covariate

That gave a model of: $$R_{i,t} = N_{i,t-2} \text{exp}\left[r - b_iN_{i,t-2} - cW_{a,t-1}+\theta_t+\theta_{a,t}+\epsilon_{i,t}\right]$$

* Variables:
  * $R_{i,t}$ is the recruitment of population $i$ in year $t$ 
  * $N_{i,t}$ is the abundance of spawners of population $i$ in year $t-2$
  * $r$ is the growth rate 
  * $b_i$ is the density-dependence parameter for each population 
  * $W_{a,t-1}$ is the average lice per wild juvenile salmon the previous year 
  * $c$ is the strength of the relationship between salmon survival and lice on wild juvenile salmon 
  * $\theta_t$ environmental stochasticity, spatially coherent among all populations for year
  * $\theta_{a,t}$ spatially coherent variation for population within a management area  
  * $\epsilon_{i,t}$ random annual variation that is independent among populations

The linear version of the Ricker model is then fit: $$ \text{ln}\frac{R_{i,t}}{N_{i,t-2}} = r - b_iN_{i,t-2} - cW_{a,t-1} + \theta_t + \theta_{a,t} + \epsilon_{i,t} $$

The Fleischman et al. (2012) version is as follows: 

$$\text{ln}(R_y) = \ln(S_y) + \ln(\alpha) - \beta S_y + \phi \omega_{y-1} + \epsilon_y$$

where the variables are:

* $S$ is spawaning escapement in year $y$ 
* $R$ is the returns (treated as unobserved states)
* $\alpha$ is the productivity parameter
* $\beta$ is the inverse capacity parameter of the Ricker stock-recruit relationship 
* $\phi$ is te autoregressive lag-1 coefficient
* $\omega_y$ is the model residual 
* $\epsilon_y$ are independent normally distributed process errors. 

We are interested in making estimates of the following parameters: 

  * $r$ is the growth rate 
  * $b_i$ is the density-dependence parameter for each population 
  * $W_{a,t-1}$ is the average lice per wild juvenile salmon the previous year 
  * $c$ is the strength of the relationship between salmon survival and lice on wild juvenile salmon 
  * $\theta_t$ environmental stochasticity, spatially coherent among all populations for year
  * $\theta_{a,t}$ spatially coherent variation for population within a management area  
  * $\epsilon_{i,t}$ random annual variation that is independent among populations