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
vw_raw <-
  names(vw_raw) <- paste0("c", seq_along(vw_raw))
