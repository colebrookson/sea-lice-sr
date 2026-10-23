#' In early 2026, some folks at PSF ran a really nice cleaning operation on
#' the NuSEDS data. Information on that is here:
#' https://zenodo.org/records/19959144 and we are going to make use of that
#' really nicely cleaned up dataset. In addition, there of course have been
#' some more data, including chunks of 2025, added since then. We'll add these
#' two sources together so we have one nice cohesive source of data.

# read in all the objects ------------------------------------------------------

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

psf_df <- readr::read_csv(here::here(cfg$path$psf_catch))
psf_cleaned_nuseds <- readr::read_csv(here::here(cfg$paths$psf_cleaned_nuseds))
nuseds <- readr::read_csv(here::here(cfg$path$nuseds))

# filter the data down and get the slightly older and newer ready to merge -----
nuseds_pinks <- nuseds |>
  dplyr::filter(SPECIES == "Pink", AREA %in% c("7":"10", "12"))
psf_nuseds_pinks <- psf_cleaned_nuseds |>
  dplyr::filter(SPECIES == "Pink", AREA %in% c("7":"10", "12"))

## check for rows differing ----------------------------------------------------
# first get the MAX_ESTIMATE in the nuseds data
count_cols <- c(
  "NATURAL_ADULT_SPAWNERS",
  "NATURAL_JACK_SPAWNERS",
  "NATURAL_SPAWNERS_TOTAL",
  "ADULT_BROODSTOCK_REMOVALS",
  "JACK_BROODSTOCK_REMOVALS",
  "TOTAL_BROODSTOCK_REMOVALS",
  "OTHER_REMOVALS",
  "TOTAL_RETURN_TO_RIVER"
)
nuseds$MAX_ESTIMATE <- do.call(pmax, c(nuseds[count_cols], na.rm = TRUE))


new25 <- nuseds |>
  dplyr::mutate(
    IndexId = paste0("PKO_", POP_ID)
  ) |>
  dplyr::left_join(xwalk, by = c("IndexId", "GFE_ID"))

dplyr::filter(new25, !is.na(MAX_ESTIMATE), is.na(streamid))
