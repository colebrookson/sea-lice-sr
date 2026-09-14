#' DESCRIPTION: this file re-formats the data to then be used by different
#' scripts that run different pieces of the modeling pipeline/process
#' AUTHOR: Cole Brookson

source(here::here("./workflow/scripts/functions/global.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

collated_df <- readr::read_csv(
  cfg$path$lice_counts
)

# we can format this where we're either going to regress on only the motiles
# OR on all the stages so we have a gate for that here

# big ugly if ------------------------------------------------------------------
if (cfg$run$motile_only) {
  # get rid of the weeks we don't want here
  collated_df_long <- collated_df |>
    dplyr::filter(week %notin% c(9, 28, 33)) |>
    dplyr::mutate(count = lep_mot) |>
    dplyr::select(obs_id, count, year, week, location) |>
    dplyr::mutate(
      # location-year from observed combinations only
      ly = factor(paste(location, year, sep = "_")),
      # factor the grouping vars AFTER
      year_f = droplevels(factor(year)),
      week_f = droplevels(factor(week)),
      ly_f = droplevels(ly),
      year_idx = as.integer(year_f),
      week_idx = as.integer(week_f),
      ly_idx = as.integer(ly_f)
    )
  stopifnot(
    !anyNA(collated_df_long$count),
    !anyNA(collated_df_long$year_idx),
    !anyNA(collated_df_long$week_idx),
    !anyNA(collated_df_long$ly_idx),
    all(collated_df_long$count == floor(collated_df_long$count))
  )

  # level maps — idx -> label, so posteriors map back
  year_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$year_f)),
    year = levels(collated_df_long$year_f)
  )
  week_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$week_f)),
    week = levels(collated_df_long$week_f)
  )
  ly_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$ly_f)),
    ly = levels(collated_df_long$ly_f)
  )
} else {
  # get rid of the weeks we don't want here
  collated_df_long <- collated_df |>
    dplyr::filter(week %notin% c(9, 28, 33)) |>
    tidyr::pivot_longer(
      cols = c(lep_mot, lep_cope, lep_chal),
      names_to = "stage",
      names_prefix = "lep_", # so values are mot/cope/chal
      values_to = "count"
    ) |>
    # drop the 2001 cope NA rows (only NA cell)
    dplyr::filter(!is.na(count)) |>
    dplyr::select(obs_id, count, stage, year, week, location) |>
    dplyr::mutate(
      # motile first so stage_idx == 1 is the reference
      stage = factor(stage, levels = c("mot", "cope", "chal")),
      # location-year from observed combinations only
      ly = factor(paste(location, year, sep = "_")),
      # factor the grouping vars AFTER
      year_f = droplevels(factor(year)),
      week_f = droplevels(factor(week)),
      ly_f = droplevels(ly)
    )

  # integer index vectors for ease
  collated_df_long <- collated_df_long |>
    dplyr::mutate(
      year_idx = as.integer(year_f),
      week_idx = as.integer(week_f),
      stage_idx = as.integer(stage),
      ly_idx = as.integer(ly_f)
    )

  year_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$year_f)),
    year = levels(collated_df_long$year_f)
  )
  week_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$week_f)),
    week = levels(collated_df_long$week_f)
  )
  stage_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$stage)),
    stage = levels(collated_df_long$stage)
  )
  ly_levels <- tibble::tibble(
    idx = seq_len(nlevels(collated_df_long$ly_f)),
    ly = levels(collated_df_long$ly_f)
  )
}

# a couple quick data checks ---------------------------------------------------
qs2::qs_save(
  collated_df_long,
  cfg$path$long_lice_counts
)
