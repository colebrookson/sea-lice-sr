#' In early 2026, some folks at PSF ran a really nice cleaning operation on
#' the NuSEDS data. Information on that is here:
#' https://zenodo.org/records/19959144 and we are going to make use of that
#' really nicely cleaned up dataset. In addition, there of course have been
#' some more data, including chunks of 2025, added since then. We'll add these
#' two sources together so we have one nice cohesive source of data.

# read in all the objects ------------------------------------------------------

cfg <- yaml::read_yaml(here::here("config/config.yaml"))

psf_cleaned <- readr::read_csv(here::here(cfg$path$psf_cleaned_nuseds))
nuseds <- readr::read_csv(here::here(cfg$path$nuseds))

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

# filter the data down and get the slightly older and newer ready to merge -----
areas <- c(7:10, 12)
nuseds_pinks <- nuseds |>
  dplyr::filter(SPECIES == "Pink", AREA %in% areas)
psf_pinks <- psf_cleaned |>
  dplyr::filter(SPECIES == "Pink", AREA %in% areas)
stopifnot(!anyDuplicated(psf_pinks[c("streamid", "Year")]))

## deal with nuseds first ------------------------------------------------------
#' PSF creates a few new data columns in their cleanup importantly, the IndexId,
#' they rename "ANALYSIS_YR" to just "Year", and then they have
#' "SPECIES_QUALIFIED" which is just which of the pink populations it is, so
#' we add those into the nuseds first

nuseds_pinks <- nuseds_pinks |>
  dplyr::mutate(
    SPECIES_QUALIFIED = dplyr::coalesce(
      substr(FULL_CU_IN, 1, 3),
      ifelse(ANALYSIS_YR %% 2 == 0, "PKE", "PKO")
    ),
    IndexId = paste0(SPECIES_QUALIFIED, "_", POP_ID),
    Year = ANALYSIS_YR
  )

# want to get all the distinct values in the psf one
comparison_vals <- psf_pinks |>
  dplyr::distinct(
    IndexId,
    GFE_ID,
    streamid,
    cuid,
    StatArea,
    sys_nm,
    SPECIES_QUALIFIED
  )

stopifnot(!anyDuplicated(comparison_vals[c("IndexId", "GFE_ID")]))

## look at all the years that PSF has already ----------------------------------
psf_ref <- psf_pinks |>
  dplyr::mutate(
    IndexId,
    GFE_ID,
    Year,
    sys_nm,
    StatArea,
    max_psf = MAX_ESTIMATE,
    in_psf = TRUE,
    .keep = "none"
  )
# and what nuseds has
keys <- c("IndexId", "GFE_ID", "Year")

new_ref <- nuseds_pinks |>
  dplyr::filter(Year <= max(psf_pinks$Year)) |>
  dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
  dplyr::summarise(
    max_new = if (all(is.na(MAX_ESTIMATE))) {
      NA_real_
    } else {
      max(MAX_ESTIMATE, na.rm = TRUE)
    },
    n_rows = dplyr::n(),
    WATERBODY = dplyr::first(WATERBODY),
    .groups = "drop"
  ) |>
  dplyr::mutate(in_new = TRUE)
stopifnot(sum(new_ref$n_rows > 1) == 0) # this should be zero!

# now we find the various versions, what is in what file
comparison_join <- dplyr::full_join(psf_ref, new_ref, by = keys) |>
  dplyr::mutate(
    dplyr::across(c(in_psf, in_new), ~ dplyr::coalesce(.x, FALSE)),
    status = dplyr::case_when(
      !in_new ~ "only in PSF",
      !in_psf ~ "only in new NuSEDS",
      is.na(max_psf) & is.na(max_new) ~ "same (both NA)",
      is.na(max_psf) ~ "count added",
      is.na(max_new) ~ "count removed",
      abs(max_new - max_psf) < 0.5 ~ "same",
      TRUE ~ "revised"
    )
  )
dplyr::count(comparison_join, status)
#' now all we rly care about is if the new row is only in NuSEDS and the value
#' is some new maximum estimate value
review <- comparison_join |>
  dplyr::filter(
    status %in%
      c("revised", "count added", "count removed") |
      (status == "only in new NuSEDS" & !is.na(max_new))
  ) |>
  dplyr::arrange(StatArea, sys_nm, Year)

## gather the data to add into the psf data ------------------------------------
to_add <- nuseds_pinks |>
  dplyr::filter(!is.na(MAX_ESTIMATE)) |>
  dplyr::left_join(
    dplyr::select(comparison_join, dplyr::all_of(keys), status),
    by = keys
  ) |>
  dplyr::filter(
    Year > max(psf_pinks$Year) |
      status %in% c("count added", "only in new NuSEDS")
  )

match_one <- to_add |>
  dplyr::inner_join(
    dplyr::select(comparison_vals, -SPECIES_QUALIFIED),
    by = c("IndexId", "GFE_ID")
  ) |>
  dplyr::mutate(match = "IndexId+GFE_ID")

# need to fill empty cells here
rest <- dplyr::anti_join(to_add, comparison_vals, by = c("IndexId", "GFE_ID"))

# get all the GFE id's that need to be brought in
comp_vals_gfe <- comparison_vals |>
  dplyr::add_count(GFE_ID, SPECIES_QUALIFIED) |>
  dplyr::filter(n == 1) |>
  dplyr::select(-n, -IndexId)

match_two <- rest |>
  dplyr::inner_join(
    comp_vals_gfe,
    by = c("GFE_ID", "SPECIES_QUALIFIED")
  ) |>
  dplyr::mutate(match = "GFE_ID+SPECIES_QUALIFIED")

unmatched <- dplyr::anti_join(
  rest,
  comp_vals_gfe,
  by = c("GFE_ID", "SPECIES_QUALIFIED")
)

additions <- dplyr::bind_rows(match_one, match_two) |>
  dplyr::mutate(
    streamid,
    GFE_ID,
    sys_nm,
    StatArea,
    cuid,
    SPECIES_QUALIFIED,
    Year,
    MAX_ESTIMATE,
    ESTIMATE_STAGE,
    added_from_nuseds = TRUE,
    match,
    .keep = "none"
  )
clash <- additions |>
  dplyr::inner_join(
    psf_pinks |>
      dplyr::filter(!is.na(MAX_ESTIMATE)) |>
      dplyr::select(streamid, Year, max_psf = MAX_ESTIMATE),
    by = c("streamid", "Year")
  )
stopifnot(nrow(clash) == 0)

spawners <- psf_pinks |>
  dplyr::mutate(
    streamid,
    GFE_ID,
    sys_nm,
    StatArea,
    cuid,
    SPECIES_QUALIFIED,
    Year,
    MAX_ESTIMATE,
    ESTIMATE_STAGE,
    added_from_nuseds = FALSE,
    match = NA_character_,
    .keep = "none"
  ) |>
  dplyr::anti_join(additions, by = c("streamid", "Year")) |>
  dplyr::bind_rows(additions) |>
  dplyr::mutate(
    area_group = ifelse(StatArea == "12", "ISC", "CC")
  ) |>
  dplyr::arrange(streamid, Year)
stopifnot(identical(
  spawners$MAX_ESTIMATE[spawners$streamid == 5738 & spawners$Year == 2021],
  7696
))

readr::write_csv(spawners, here::here(cfg$path$spawners))
