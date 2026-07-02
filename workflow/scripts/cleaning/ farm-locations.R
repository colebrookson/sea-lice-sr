#' DESCRIPTION: Taking all the farm location  data and put it together and save
#' it so it's easily accessible in the future
#' AUTHOR: Cole Brookson
#' DATE: 18 January 2026

# all farm locations read in, get the locations for Wa-kwa and Tsa-ya
all_farm_locs <- readr::read_csv(
    here::here("./data/broughton-farm-data/farm-location-data-raw.csv")
) |>
    dplyr::rename(
        lat = Latitude,
        long = Longitude
    ) |>
    dplyr::select(ref, name, lat, long)

wakwa_tsaya <- all_farm_locs |>
    dplyr::filter(name %in% c("Tsa-ya", "Wa-kwa")) |>
    dplyr::rename(farm_name = name) |>
    dplyr::mutate(farm_num = c(27, 28)) |>
    dplyr::select(farm_name, farm_num, lat, long)


# read in the version that I made
older_farm_locs <- readr::read_csv(
    here::here("./data/broughton-farm-data/farm-lat-long.csv")
)

# stich the two farm location dfs together
all_farm_locations <- rbind(older_farm_locs, wakwa_tsaya)
readr::write_csv(
    all_farm_locations,
    here::here("./data/broughton-farm-data/farm-locations.csv")
)
