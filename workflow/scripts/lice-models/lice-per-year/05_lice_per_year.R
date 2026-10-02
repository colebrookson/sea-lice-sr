#' DESCRIPTION: fit the model that's written in stan to get the estimated
#' values (without doing a joint fit) of the lice per year in the various ways

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

collated_df_long <- qs2::qs_read(here::here(cfg$path$long_lice_counts))

model <- cmdstanr::cmdstan_model(
  cfg$path$models$lice_per_year
)
model$print()

# set the data list
data_list <- list(
  N = nrow(collated_df_long), # number of obs
  N_ys = length(unique(collated_df_long$ys_f)), # no. of year x stage
  N_ly = length(unique(collated_df_long$ly_f)), # location year combos
  N_s = length(unique(collated_df_long$stage)), # number of stages
  N_wk = length(unique(collated_df_long$week)), # weeks
  y = collated_df_long$count, # response data
  ys_idx = collated_df_long$ys_idx, # gets the index of that 1,...,N_ys
  ly_idx = collated_df_long$ly_idx, # gets the index of that 1,...,N_ly
  stage_idx = collated_df_long$stage_idx, # different, just the three stages
  wk_idx = collated_df_long$week_idx # which week
)

fit_model <- model$sample(
  data = data_list,
  seed = cfg$run$yearly_model$seed,
  chains = cfg$run$yearly_model$chains,
  parallel_chains = cfg$run$yearly_model$parallel_chains,
  refresh = cfg$run$yearly_model$refresh,
  iter_sampling = cfg$run$yearly_model$iter_sampling,
  iter_warmup = cfg$run$yearly_model$iter_warmup
)
#' running this with 1000 iter, 500 warmup took 4210.8s
qs2::qs_save(fit_model, here::here(cfg$path$mod_obs$lice_per_year))
