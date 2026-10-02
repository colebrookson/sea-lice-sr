source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

collated_df_long <- qs2::qs_read(here::here(cfg$path$long_lice_counts))

if (cfg$run$yearly_model$subsample) {
  #' if we want to try the model with a sub-set, we need to sub-sample
  #' fish (aka don't split fish across stages), so we want to stratefy
  #' across the sub-groups

  set.seed(cfg$run$seed)

  frac <- 0.1

  # what are the fish id's to keep?
  keep_ids <- collated_df_long |>
    dplyr::distinct(obs_id, year) |>
    dplyr::group_by(year) |>
    dplyr::slice_sample(prop = frac) |>
    dplyr::pull(obs_id)

  collated_df_long <- collated_df_long |>
    dplyr::filter(obs_id %in% keep_ids) |>
    dplyr::mutate(
      ys_idx = as.integer(factor(ys_f)),
      ly_idx = as.integer(factor(ly_f)),
      wk_idx = as.integer(factor(week)),
      stage_idx = as.integer(factor(stage))
    )

  # every year and stage must be present otherwise doesn't work
  stopifnot(
    dplyr::n_distinct(collated_df_long$year) ==
      dplyr::n_distinct(collated_df_long$year),
    dplyr::n_distinct(collated_df_long$stage) == 3,
    max(collated_df_long$ys_idx) == dplyr::n_distinct(collated_df_long$ys_f),
    max(collated_df_long$ly_idx) == dplyr::n_distinct(collated_df_long$ly_f),
    max(collated_df_long$wk_idx) == dplyr::n_distinct(collated_df_long$week)
  )
}

model <- cmdstanr::cmdstan_model(
  cfg$path$models$lice_per_year
)
model$print()
model_looped <- cmdstanr::cmdstan_model(
  "./workflow/models/lice_per_year_looped.stan"
)

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
fit_looped <- model_looped$sample(
  data = data_list,
  seed = cfg$run$yearly_model$seed,
  chains = cfg$run$yearly_model$chains,
  parallel_chains = cfg$run$yearly_model$parallel_chains,
  refresh = cfg$run$yearly_model$refresh,
  iter_sampling = cfg$run$yearly_model$iter_sampling,
  iter_warmup = cfg$run$yearly_model$iter_warmup
)

# check these are right !!

model_v <- cmdstanr::cmdstan_model(
  cfg$path$models$lice_per_year,
  compile_model_methods = TRUE,
  force_recompile = TRUE
)
model_l <- cmdstanr::cmdstan_model(
  "./workflow/models/scratch/lice_per_year_looped.stan",
  compile_model_methods = TRUE,
  force_recompile = TRUE
)

fit_v_tiny <- model_v$sample(
  data = data_list,
  seed = 1,
  chains = 1,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 500,
  sig_figs = 18
)
fit_l_tiny <- model_l$sample(
  data = data_list,
  seed = 1,
  chains = 1,
  refresh = 100,
  iter_sampling = 1000,
  iter_warmup = 500,
  sig_figs = 18
)

fit_v_tiny$init_model_methods()
fit_l_tiny$init_model_methods()
ud <- fit_v_tiny$unconstrain_draws(format = "draws_matrix")
um <- unclass(ud)[1:20, , drop = FALSE]

lp_v <- apply(um, 1, fit_v_tiny$log_prob)
lp_l <- apply(um, 1, fit_l_tiny$log_prob)

max(abs(lp_v - lp_l)) # this needs to be zero
