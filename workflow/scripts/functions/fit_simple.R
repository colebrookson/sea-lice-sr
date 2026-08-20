#' fit_extract_plot
#'
#' @description fit a model between number of lice on wild fish vs farmed fish
#'
#' @param df the dataframe you want to use, should be of the mean number of the
#' two variables per-year
#' @param wild_lice the variable with the yearly counts of lice on wild fish
#' @param farm_lice the variable with the yearly counts of lice on farm fish
#' @param wild_choice a readable string noting which variable is being used
#' @param farm_choice a readable string noting which variable is being used and
#' which farms are included
#' @param slug the short slug to call the plot output
#'
#' @return

fit_extract_plot <- function(
  df,
  wild_lice,
  farm_lice,
  wild_choice,
  farm_choice,
  slug
) {
  # df = yearly_lice_data
  # wild_lice = "mean_all_leps"
  # farm_lice = "mean_marty_lep_tot"
  # slug = "-mean-marty-"
  formula <- reformulate(farm_lice, response = wild_lice)
  simple_mod <- stats::lm(formula, data = df)
  r2_val <- summary(simple_mod)$r.squared
  plot_lab <- paste("R^2 == ", round(r2_val, 2))
  # wild_farm_reg <- ggplot(data = df) +
  #     geom_point(
  #         aes(
  #             x = rlang::ensym(farm_lice), y = rlang::ensym(wild_lice)
  #         ),
  #         size = 4, shape = 21, colour = "black", fill = "#42e4e4"
  #     ) +
  #     geom_smooth(aes(x = rlang::ensym(farm_lice),
  #     y = rlang::ensym(wild_lice)),
  #         formula = y ~ x, method = "lm", level = 0.95
  #     ) +
  #     labs(x = "Lice on Farmed Fish (millions)", y = "Lice on Wild Fish") +
  #     scale_x_log10(
  #         breaks = c(3e+05, 1e+06, 3e+06),
  #         labels = c("0.3", "1.0", "3.0")
  #     ) +
  #     scale_y_log10() +
  #     theme_better() +
  #     annotate("text",
  #         x = 0.3e+06, y = 3, label = plot_lab, parse = TRUE,
  #         size = 10
  #     )

  # ggsave(
  #     paste0(here::here("./figs/wild-vs-farm/wild-farm-regression"),
  #     slug, ".png"),
  #     wild_farm_reg,
  #     dpi = 300,
  #     height = 8, width = 11
  # )
  return(r2_val)
}

fit_all_farms_marty <- fit_extract_plot(
  df = yearly_lice_data,
  wild_lice = "mean_all_leps",
  farm_lice = "marty_mean_lep_tot",
  slug = "-mean-marty-"
)

hist(yearly_lice_data$mean_all_leps)
# A wrapper that turns text strings into formulas
run_model_from_text <- function(dataset, target, predictor) {
  # Create a formula from strings: target ~ predictor
  my_formula <- reformulate(predictor, response = target)

  # Print the formula to verify
  print(my_formula)

  # Run the model
  lm(my_formula, data = dataset)
}

# Call the function with strings
run_model_from_text(dataset = mtcars, target = "mpg", predictor = "wt")
