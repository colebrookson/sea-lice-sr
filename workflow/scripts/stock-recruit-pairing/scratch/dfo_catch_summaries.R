source(here::here("./workflow/scripts/functions/theme_better.R"))
df <- readr::read_csv(here::here(
  "./data/dfo-data/raw/pink/transcribed-dfo-catch-summaries.csv"
)) |>
  tidyr::pivot_longer(
    cols = c(
      "Area A",
      "Area B",
      "Area C",
      "Area D",
      "Area E",
      "Area F",
      "Area G",
      "Area H"
    ),
    values_to = "catch",
    names_to = "area"
  ) |>
  dplyr::mutate(
    pfma = dplyr::case_when(
      area %in% c("Area A", "Area C", "Area F") ~ "7-10",
      area %in% c("Area B", "Area D", "Area G", "Area H") ~ "12",
      TRUE ~ "Neither"
    )
  )

library(ggplot2)

ggplot(data = df) +
  geom_line(
    aes(x = Year, y = catch, colour = pfma)
  ) +
  geom_point(
    aes(x = Year, y = catch, fill = pfma),
    colour = "black",
    shape = 21
  ) +
  facet_wrap(~area, ncol = 4, scales = "free_y") +
  scale_x_continuous(
    breaks = unique(df$Year),
    labels = unique(df$Year)
  ) +
  theme_better() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12)
  )

ggplot(data = df[which(df$Year > 2021), ]) +
  geom_line(
    aes(x = Year, y = catch, colour = pfma)
  ) +
  geom_point(
    aes(x = Year, y = catch, fill = pfma),
    colour = "black",
    shape = 21
  ) +
  facet_wrap(~area, ncol = 4) +
  scale_x_continuous(
    breaks = unique(df$Year),
    labels = unique(df$Year)
  ) +
  theme_better() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 12)
  )
