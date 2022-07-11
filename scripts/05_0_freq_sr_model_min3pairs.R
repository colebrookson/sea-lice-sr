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
library(PNWColors)

# pull in file with all functions to clean data 
source(here::here("./src/01_plot_themes.R"))

sr_df = readr::read_csv(here::here(
  "./data/for-model-runs/stock-recruit-data-cut-off-03.csv"
))
cat("Final dataset: \n Total number of populations (even/odd): ", 
    length(unique(sr_df$pop)), "\n Total number of S-R pairs: ", 
    dim(sr_df)[1], "\n Total number of rivers: ", 
    length(unique(sr_df$river)))

# last touch-up's of the data ==================================================

sr_df[which(sr_df$area == 12 & sr_df$year %in% c(1991:2001)), "lice"] = NA
sr_df = subset(sr_df, is.na(lice)==FALSE)
#sr_df = sr_df[which(sr_df$year > 1961),]

sr_df$area = as.factor(sr_df$area)
sr_df$year_fac = as.factor(sr_df$year)
sr_df$population_name = as.factor(sr_df$population_name)

# run models ===================================================================

null_model = lme4::lmer(log_survival ~ S:population_name +
                                (1|year_fac/area),
                              data = sr_df)
saveRDS(null_model, here::here(
  "./data/model-outputs/stock-recruit-null-model.RDS"
))
alt_model = lme4::lmer(log_survival ~ S:population_name +
                               lice + (1|year_fac/area),
                             data = sr_df)
saveRDS(alt_model, here::here(
  "./data/model-outputs/stock-recruit-alternative-model.RDS"
))

summary(null_model)
summary(alt_model)
anova(null_model, alt_model)

cat("with lmer(), value of fitted r (growth rate) is ", 
    lme4::fixef(alt_model)[1],
    " and the value of c (effect of sea lice) is ", lme4::fixef(alt_model)[2])

# bootstrap confidence intervals ===============================================

# make columns for the fixed and random effects
sr_df$year_area = rep(0, nrow(sr_df))
sr_df$r = rep(0, nrow(sr_df))

# get the different levels of the random effect 
rand_effects = rownames(lme4::ranef(alt_model)$`area:year_fac`)
fixed_effects = names(lme4::fixef(alt_model))[3:length(lme4::fixef(alt_model))]

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
    a + 
      b_i[sr_df$r] * sr_df$S + 
      c * sr_df$lice + 
      theta_ya[sr_df$year_area] + 
      theta_y[as.numeric(sr_df$year_fac)] +
      epsilon)
  
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
  model = lme4::lmer(survival ~ S:population_name + 
                             lice + (1|year_fac/area),
                           data = temp_sr_df, REML = TRUE)
  
  # get the results 
  params_result = lme4::fixef(model)[1:2]
  return(params_result)
}

# set up estimated variances for bootstrap algorithm
b = as.numeric(lme4::fixef(alt_model)[3:length(lme4::fixef(alt_model))])
a = as.numeric(lme4::fixef(alt_model)[1]) # intercept 
c = as.numeric(lme4::fixef(alt_model)[2]) # lice

# get the standard deviations 
sigma_ya = attr(lme4::VarCorr(alt_model)$`area:year_fac`, "stddev")[[1]]
sigma_y = attr(lme4::VarCorr(alt_model)$`year`, "stddev")[[1]]
sigma_e = attr(lme4::VarCorr(alt_model), "sc")

parameters = list(a, b, c, sigma_ya, sigma_y, sigma_e)

# run this all in parallel 
cores = detectCores()-1 # keep one for processing other things 

# get random sequences for different chains 
n_jobs = 1000
RNGkind("L'Ecuyer-CMRG")
set.seed(1234)
job_seeds = matrix(nrow = n_jobs, ncol = 7)
job_seeds[1,] = .Random.seed

for(i in 2:n_jobs) job_seeds[i,] = parallel::nextRNGStream(job_seeds[i-1,])

t0 = proc.time()
cl = parallel::makeCluster(cores)
parallel::clusterExport(cl, varlist = list("job_seeds", "sr_df", "parameters"))
output = parallel::clusterApply(cl, x = c(1:n_jobs), fun = bootstrap)

saveRDS(output, here::here("./data/model-outputs/parallel-bootstrapping.RDS"))
output = readRDS(
  here::here("./data/model-outputs/parallel-bootstrapping.RDS")
)
cat("Process time (minutes) = ", (proc.time()-t0)[3]/60)

# unlist all results 
p_all = matrix(nrow = n_jobs, ncol = 2)
for(i in 1:n_jobs) p_all[i,] = as.numeric(output[[i]])

# now make the actual confidence intervals 
ci = apply(p_all, 2, quantile, c(0.025, 0.975))
ci = rbind(ci[1,], as.numeric(
  lme4::fixef(alt_model)[1:2]), ci[2,]) 
colnames(ci) = c("r", "c")
rownames(ci) = c("2.5%", "MLE", "97.5%")
print(ci)

# get percent mortality estimates ==============================================

# make a df for the louse values 
lice_df = unique(sr_df[which(sr_df$area == 12 & 
                               sr_df$year > 2001), c("lice", "year")])

# get mortality extimates
mortality = cbind(ci[2,2]*lice_df$lice, # mle
            ci[1,2]*lice_df$lice, # 2.5%
            ci[3,2]*lice_df$lice) # 97.5%
# percentage mortality 
p_mort = 100*(1-exp(mortality))
colnames(p_mort) = c("MLE", "upper", "lower")
rownames(p_mort) = c(2002:2016)
p_mort = data.frame(p_mort)
p_mort$year = rownames(p_mort)

# make plots ===================================================================

# plot survival first
point_plot_df = data.frame(
  survival = sr_df$log_survival,
  x_val = sr_df$S*10^(-6),
  area = sr_df$area,
  year = sr_df$year,
  group = character(length(sr_df$log_survival))
)
point_plot_df[which(point_plot_df$area != 12), "group"] = "Non-area 12"
point_plot_df[which(point_plot_df$area == 12 & 
                      point_plot_df$year < 2002), "group"] = "Area 12, pre-lice"
point_plot_df[which(point_plot_df$area == 12 & 
                      point_plot_df$year >= 2002), "group"] = "Area 12, lice"

non_12 = point_plot_df[which(point_plot_df$group == "Non-area 12"),]
no_lice_12 = point_plot_df[which(point_plot_df$group == "Area 12, pre-lice"),]
lice_12 = point_plot_df[which(point_plot_df$group == "Area 12, lice"),]

# for the lice_12 dataset, label the outliers on the bottom 
lice_12$label = "NA"
non_12$label = ""
no_lice_12$label = ""


# NOTE ######
# in the labeling process below, the -1 is to show the labels as the out year 
# not the return year 
# END NOTE ######
for(row in seq_len(nrow(lice_12))) {
  if(lice_12[row, "x_val"] > 1){
    lice_12[row, "label"] = lice_12[row, "year"] -1
  } else if((lice_12[row, "x_val"] > 0.37) & 
             (lice_12[row, "survival"] < -2.5)) {
    lice_12[row, "label"] = lice_12[row, "year"] -1
  } else if(lice_12[row, "survival"] < -6) {
    lice_12[row, "label"] = lice_12[row, "year"] -1
  } else {
    lice_12[row, "label"] = ""
  }
}



# bind df all together
surv_df = rbind(non_12, no_lice_12, lice_12)

ggplot(data = surv_df, aes(x = x_val, y = survival, label = label)) + 
  geom_point(aes(fill = group, size = group, colour = group, alpha = group), 
             shape = 21) + 
  theme_area_grouping() + 
  scale_fill_manual(" ", values = c("purple", "goldenrod2", "grey60")) + 
  scale_alpha_manual(values = c(0.8, 0.8, 0.6)) + 
  scale_colour_manual(values = c("white", "white", "white")) + 
  scale_size_manual(values = c(4, 3, 3)) + 
  labs(
    x = bquote("Spawner Abundance "(x10^6)),
    y = "Survival"
  ) +
  geom_text(hjust = -0.2, vjust = 0.5) + 
  guides(
    fill = guide_legend(override.aes = list(fill = c("purple", "goldenrod2", "grey60"))),
    size = FALSE,
    colour = FALSE,
    alpha = FALSE
  )

data_spread = ggplot() + 
  geom_point(data = non_12, 
             aes(x = x_val, y = survival, fill = group), colour = "white",
             alpha = 0.8, shape = 21, size = 3) + 
  geom_point(data = no_lice_12,
             aes(x = x_val, y = survival, fill = group), colour = "white",
             alpha = 0.8, shape = 21, size = 3) + 
  geom_point(data = lice_12,
             aes(x = x_val, y = survival, fill = group), 
             alpha = 0.6, size = 4, shape = 21) + 
  geom_text(data = lice_12, hjust = 0, vjust = 0, label = label) +
  theme_area_grouping() + 
  scale_fill_manual(" ", values = c("purple", "goldenrod2", "grey60")) + 
  labs(
    x = bquote("Spawner Abundance "(x10^6)),
    y = "Survival"
  )
ggsave(here::here("./figs/three-classes-of-data.png"), data_spread,
       width = 10, height = 5)

# plot the estimated mortalities
est_mortality = ggplot(data = p_mort) +
  geom_errorbar(aes(x = year, ymin = lower, ymax = upper), width = 0) +
  geom_point(aes(x = year, y = MLE, fill = MLE),
             colour = "black", shape = 21, size = 5) +
  theme_farm_grouping() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  ) +
  labs(x = "Year", y = "Max. Like. Estimate & 95% CI's") + 
  scale_fill_gradientn(colours = rev(PNWColors::pnw_palette("Sunset2",
                                                          type = "continuous")))
ggsave(here::here("./figs/estimated-mortality.png"), est_mortality,
       height = 7, width = 10)



