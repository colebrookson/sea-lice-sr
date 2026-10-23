#' I want all the exploitation rates in one single file for use since they come
#' from annoyingly random places

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

psf_df <- readr::read_csv(here::here(cfg$path$psf_catch))
# filter data down to what I want ----------------------------------------------

pinks_only <- psf_cleaned_nuseds |> dplyr::filter(SPECIES == "Pink")
readr::write_csv(
  pinks_only,
  file = here::here("./data/PSF-PSE/cleaned-nuseds/pinks-only.csv ")
)
