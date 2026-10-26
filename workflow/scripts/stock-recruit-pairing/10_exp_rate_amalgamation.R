#' I want all the exploitation rates in one single file for use since they come
#' from annoyingly random places

source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
cfg <- yaml::read_yaml(here::here("config/config.yaml"))

psf_catch <- readr::read_csv(here::here(cfg$path$psf_catch))
area_12 <- readxl::read_excel(
  here::here(cfg$path$van_will_reconstruction),
  sheet = "Summary",
  col_names = FALSE,
  .name_repair = "minimal"
)
names(area_12) <- paste0("c", seq_along(area_12))
spawners <- readr::read_csv(here::here(cfg$path$nuseds_amalgamed))

# deal with the PSF data first -------------------------------------------------
cc_rates <- psf_catch |>
  dplyr::filter(
    region == "Central Coast",
    species_qualified %in% c("PKE", "PKO")
  ) |>
  dplyr::mutate(exploitation_rate = dplyr::na_if(exploitation_rate, -989898)) |>
  dplyr::filter(!is.na(exploitation_rate)) |>
  dplyr::mutate(
    area_group = "CC",
    cuid,
    return_year = year,
    u = exploitation_rate,
    u_source = "PSF",
    .keep = "none"
  )
# stopifnot(setequal(unique(cc_rates$cuid), 608:612))

# van will data ----------------------------------------------------------------
#' need to clean this up since it's clearly not formated right
header_even <- 1
header_odd <- which(area_12$c2 == "Year")

row_years <- function(r) suppressWarnings(as.numeric(unlist(area_12[r, ])))
#' because of the odd structure of the actual excel sheet, I need
#' to use this kind of lookup table to figure out which rows correspond to what.
#' previously i'd done this manually in a spreadsheet before reading it in,
#' but would rather be """"reproducbile"""" about it
year_lookup <- dplyr::bind_rows(
  tibble::tibble(
    block = "even",
    col = names(area_12),
    year = row_years(header_even)
  ),
  tibble::tibble(
    block = "odd",
    col = names(area_12),
    year = row_years(header_odd)
  )
) |>
  dplyr::filter(!is.na(year), year > 1900) # drops Median / 40th / 25th columns

area_12_cuid <- tibble::tibble(
  cu = c(
    "Southern Fjords (even)",
    "Southern Fjords (odd)",
    "East Vancouver Island (odd)",
    "Nahwitti",
    "Homathko-Klinaklini (odd)"
  ),
  cuid = c(920, 926, 923, 924, 610)
)

area_12_rates <- area_12 |>
  dplyr::mutate(row = dplyr::row_number(), cu = trimws(c1)) |>
  tidyr::fill(cu) |>
  dplyr::filter(c2 == "Exploitation") |>
  dplyr::mutate(
    block = ifelse(row < header_odd, "even", "odd"),
    dplyr::across(
      dplyr::all_of(unique(year_lookup$col)),
      ~ as.numeric(.x)
    )
  ) |>
  tidyr::pivot_longer(
    dplyr::all_of(unique(year_lookup$col)),
    names_to = "col",
    values_to = "u"
  ) |>
  dplyr::inner_join(year_lookup, by = c("block", "col")) |>
  # drops Aggregate, Georgia Strait, East Howe Sound
  dplyr::inner_join(area_12_cuid, by = "cu") |>
  dplyr::filter(!is.na(u)) |>
  dplyr::mutate(
    area_group = "ISC",
    cuid,
    return_year = year,
    u,
    u_source = "Van Will 2021",
    .keep = "none"
  )
stopifnot(setequal(unique(area_12_rates$cuid), area_12_cuid$cuid))

# add in the later years to the data -------------------------------------------
#' we're assuuming a 0% and 5% incidental harvest rate for 2022-2025 throughout
#' the entire region we're interested in
fill_rates <- tidyr::expand_grid(
  dplyr::distinct(dplyr::bind_rows(cc_rates, area_12_rates), area_group, cuid),
  return_year = 2022:2025
) |>
  dplyr::mutate(
    u = ifelse(return_year %% 2 == 1, 0.05, 0),
    u_source = "assumed"
  )
rates <- dplyr::bind_rows(cc_rates, area_12_rates, fill_rates)

sr <- spawners |>
  dplyr::left_join(
    rates,
    by = c("area_group", "cuid", "Year" = "return_year")
  ) |>
  dplyr::mutate(returns = MAX_ESTIMATE / (1 - u)) |>
  dplyr::arrange(streamid, Year) |>
  dplyr::group_by(streamid) |>
  dplyr::mutate(recruits = returns[match(Year + 2, Year)]) |>
  dplyr::ungroup() |>
  dplyr::mutate(log_rs = log(recruits / MAX_ESTIMATE))

stopifnot(nrow(sr) == nrow(spawners))

readr::write_csv(
  sr,
  here::here(cfg$path$pink_sr_unfiltered)
)
