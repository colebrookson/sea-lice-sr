# Generates minimal synthetic CSVs for the test pipeline run.
set.seed(42)
n <- 20
dates <- seq(as.Date("2020-01-01"), by = "month", length.out = n)

# data_type_1: simple time series ----------------------------------------------
data_type_1 <- tibble::tibble(
    date = dates,
    value = round(rnorm(n, mean = 10, sd = 2), 2),
    group = rep(c("A", "B"), length.out = n),
    location = "site_1"
)

out_1 <- "test/resources/data_type_1/test_data_type_1.csv"
dir.create(dirname(out_1), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(data_type_1, out_1)

# data_type_2: similar structure, includes some NAs ----------------------------
data_type_2 <- tibble::tibble(
    date = dates,
    value = round(rnorm(n, mean = 50, sd = 5), 2),
    group = rep(c("X", "Y"), length.out = n),
    location = "site_2"
)

# Sprinkle a couple of NAs to exercise the drop_na = false path
data_type_2$group[c(3, 11)] <- NA

out_2 <- "test/resources/data_type_2/test_data_type_2.csv"
dir.create(dirname(out_2), recursive = TRUE, showWarnings = FALSE)
readr::write_csv(data_type_2, out_2)
