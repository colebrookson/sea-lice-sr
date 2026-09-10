#' DESCRIPTION: Scenario 5, which is no species imputation at all, response is
#' every louse (both species, every stage)
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

library(ggplot2)

fish_df <- readr::read_csv(
  cfg$path$clean_fish
)
# all_lice as response ---------------------------------------------------------
scenario5 <- fish_df |>
  dplyr::mutate(
    week = lubridate::isoweek(lubridate::make_date(year, month, day)),
    lice_for_model = all_lice
  ) |>
  dplyr::select(
    obs_id,
    year,
    month,
    day,
    week,
    location,
    all_lice,
    lice_for_model
  )

# all_lice is a rowSums(..., na.rm = TRUE) so it is never NA; assert it --------
stopifnot(!any(is.na(scenario5$lice_for_model)))

readr::write_csv(scenario5, cfg$path$scenarios$scen_5)
