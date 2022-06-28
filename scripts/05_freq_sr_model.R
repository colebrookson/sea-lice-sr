##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-06-03
##########
##########

# set up =======================================================================

library(tidyverse)
library(here)
library(glmmTMB)
library(parallel)

# pull in file with all functions to clean data 
source(here::here("./src/01_plot_themes.R"))

sr_df = readr::read_csv(here::here(
  "./data/for-model-runs/stock-recruit-data.csv"
))
unfilter_sr_df= readr::read_csv(here::here(
  "./data/for-model-runs/non-filtered-stock-recruit-data.csv"
))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(sr_df)[1], "\n Total number of rivers: ", 
    length(unique(sr_df$river)))

cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(unfilter_sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(unfilter_sr_df)[1], "\n Total number of rivers: ", 
    length(unique(unfilter_sr_df$river)))
# last touch-up's of the data ==================================================

sr_df[which(sr_df$area == 12 & sr_df$year %in% c(1991:2001)), "lice"] = NA
sr_df = subset(sr_df, is.na(lice)==FALSE)
#sr_df = sr_df[which(sr_df$year > 1961),]

sr_df$area = as.factor(sr_df$area)
sr_df$year_fac = as.factor(sr_df$year)
sr_df$population_name = as.factor(sr_df$population_name)

# do it again for unfiltered data 
#unfilter_sr_df[which(unfilter_sr_df$area == 12 & 
#                       unfilter_sr_df$year %in% c(1990:2000)), "lice"] = NA
#unfilter_sr_df = unfilter_sr_df[which(unfilter_sr_df$year > 1961),]

unfilter_sr_df$area = as.factor(unfilter_sr_df$area)
unfilter_sr_df$year_fac = as.factor(unfilter_sr_df$year)
unfilter_sr_df$population_name = as.factor(unfilter_sr_df$population_name)

# remove pop's with less than 4 pairs since that's the min to fit a plane
unfilter_sr_df = unfilter_sr_df[which(
  unfilter_sr_df$pop > 4), ]

# run models ===================================================================

null_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                (1|year_fac/area),
                              data = sr_df)
saveRDS(null_model, here::here(
  "./data/model-outputs/stock-recruit-null-model.RDS"
))
alt_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                               scale(lice) + (1|year_fac/area),
                             data = sr_df)
saveRDS(alt_model, here::here(
  "./data/model-outputs/stock-recruit-alternative-model.RDS"
))

summary(null_model)
summary(alt_model)
anova(null_model, alt_model)

cat("value of fitted r (growth rate) is ", alt_model$fit$par[[1]],
      " and the value of c (effect of sea lice) is ", alt_model$fit$par[[2]])

# attempt to fit models with observations not excluded 

unfilter_null_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                (1|year_fac/area),
                              data = unfilter_sr_df)
saveRDS(unfilter_null_model, here::here(
  "./data/model-outputs/unfiltered-data-stock-recruit-null-model.RDS"
))
unfilter_alt_model = glmmTMB::glmmTMB(log_survival ~ scale(S):population_name +
                                        scale(lice) + 
                                        (1|year_fac/area),
                                      data = unfilter_sr_df)
saveRDS(alt_model, here::here(
  "./data/model-outputs/unfiltered-data-stock-recruit-alternative-model.RDS"
))

summary(unfilter_null_model)
summary(unfilter_alt_model)
anova(unfilter_null_model, unfilter_alt_model)

# bootstrap confidence intervals ===============================================

# make columns for the fixed and random effects
sr_df$year_area = rep(0, nrow(sr_df))
sr_df$r = rep(0, nrow(sr_df))

# get the different levels of the random effect 
rand_effects = rownames(glmmTMB::ranef(alt_model)$cond$`area:year_fac`)
fixed_effects = names(fixef(alt_model)$cond)[3:length(fixef(alt_model)$cond)]

# loop to get the values from the fitted model object
for(i in 1:length(rand_effects)) {
  
  # split the random effect of area:year_fac
  rand_ef = stringr::str_split(rand_effects[i], ":")
  
  # subset data to get the value
  sr_df[which(sr_df$area == rand_ef[[1]][1] & 
                sr_df$year_fac == rand_ef[[1]][2]),"year_area"] = i
}

# now loop through to get the values of the fixed effect from the fitted model 
for(i in 1:length(fixed_effects)) {
  
  # split the string 
  fixed_ef = stringr::str_remove(
    stringr::str_split(fixed_effects[i], ":")[[1]][2], "population_name"
  )
  
  # subset data to put the value in the matching location 
  sr_df[which(sr_df$population_name == fixed_ef), "r"] = i
}

bootstrap = function(x) {
  
  # set the values that we want 
  a = parameters[[1]]; b_i = parameters[[2]]; c = parameters[[3]]; 
  sigma_ya = parameters[[4]]; sigma_y = parameters[[5]]
  sigma_e = parameters[[6]]
  
  # set a seed 
  set.seed(job_seeds[x,2])
  
  R = numeric(length(sr_df$S)) # this will be simulated survival
  theta_y = rnorm(length(levels(sr_df$year_fac)), 0, sigma_y)
  theta_ya = rnorm(max(sr_df$year_area), 0, sigma_ya)
  epsilon = rnorm(length(sr_df$S), 0, sigma_e)
  
  # calculate R 
  R = sr_df$S * exp(
      scale(a + 
      b_i[sr_df$r] * sr_df$S + 
        c * sr_df$lice + 
        theta_ya[sr_df$year_area] + 
        theta_y[as.numeric(sr_df$year_fac)] +
        epsilon))
  
  # get survival 
  SS = log(R/sr_df$S)
  
  # make a temporary dataframe 
  temp_sr_df = data.frame(
    area = as.factor(sr_df$area),
    population_name = as.factor(sr_df$population_name),
    year_fac = as.factor(sr_df$year_fac),
    S = sr_df$S,
    survival = SS,
    lice = sr_df$lice
  )
  
  # actually fit the model
  model = glmmTMB::glmmTMB(survival ~ scale(S):population_name + 
                             scale(lice) + (1|year_fac/area),
                           data = temp_sr_df, REML = TRUE)
  
  # get the results 
  params_result = glmmTMB::fixef(model)[1:2]
}

# set up estimated variances for bootstrap algorithm
b = as.numeric(glmmTMB::fixef(alt_model)$cond[3:length(fixef(alt_model)$cond)])
a = as.numeric(glmmTMB::fixef(alt_model)$cond[1]) # intercept 
c = as.numeric(glmmTMB::fixef(alt_model)$cond[2]) # lice

# get the standard deviations 
sigma_ya = attr(VarCorr(alt_model)[[1]]$`area:year_fac`, "stddev")[[1]]
sigma_y = attr(VarCorr(alt_model)[[1]]$`year`, "stddev")[[1]]
sigma_e = attr(VarCorr(alt_model)[[1]], "sc")

parameters = list(a, b, c, sigma_ya, sigma_y, sigma_e)

# run this all in parallel 
cores = detectCores()-1 # keep one for processing other things 

# get random sequences for different chains 
n_jobs = 2
RNGkind("L'Ecuyer-CMRG")
set.seed(1234)
job_seeds = matrix(nrow = cores, ncol = 7)
job_seeds[1,] = .Random.seed

for(i in 2:n_jobs) job_seeds[i,] = parallel::nextRNGStream(job_seeds[i-1,])

t0 = proc.time()
cl = parallel::makeCluster(cores)
parallel::clusterExport(cl, carlist = list("job_seeds", "sr_df", "parameters"))
output = parallel::clusterApply(cl, x = c(1:1000), fun = bootstrap)

saveRDS(output, here::here("./data/model-outputs/parallel-bootstrapping.RDS"))
cat("Process time (minutes) = ", (proc.time()-t0)[3]/60)

# unlist all results 
p_all = matrix(nrow = 1000, ncol = 2)
for(i in 1:10000) p_all[i,] = as.numeric(output[[i]])

# now make the actual confidence intervals 
ci = apply(p_all, 2, quantile, c(0.025, 0.975))
ci = rbind(ci[1,], as.numeric(
  glmmTMB::fixef(alt_model)[[1]][1:2]), ci[2,]) 
colnames(ci) = c("r", "c")
rownames(ci) = c("2.5%", "MLE", "97.5%")
print(ci)


