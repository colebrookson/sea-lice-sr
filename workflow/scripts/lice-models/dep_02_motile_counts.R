#' DESCRIPTION: We need to come up with a count of the number of L. salmonis
#' per wild fish, so we estimate the number of lice on wild juvenile salmon
#' by accounting for the number of motile, chalimus, and copepodite lice
#' sepeartely, using year-level predictions.
#' DATE: 20 January 2026
#' AUTHOR: Cole

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
library(magrittr)

fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)

# part 2 -----------------------------------------------------------------------

#' part 2 - for the years where motiles were mostly speciated, 2002 - present,
#' estimate the number of unidentified motiles that were actually L.
#' salmonis using the empirical proportion of L. salmonis among the speciated
#' motiles

df_2002_onwards <- fish_df %>%
    dplyr::filter(year > 2001) %>%
    dplyr::filter(all_mot > 0)
df_2001 <- fish_df %>% dplyr::filter(year == 2001)

## part 2.1 - regression on the motiles ----------------------------------------
model <- stats::glm(cbind(lep_mot, all_sp_mot - lep_mot) ~ all_mot,
    # binomial family
    family = binomial(link = "logit"),
    data = df_2002_onwards
)
coefs <- broom::tidy(model)
model_vals <- broom::glance(model)

## part 2.2 - model prediction -------------------------------------------------
#' Take in the fitted model object and make predictions for the values that
#' the model actually covers out to the maximum count of number of motiles

# find the maximum number of motiles
max_mot <- max(df_2002_onwards$all_mot)

# make sequence of values ot predict on
mot_seq <- data.frame(all_mot = seq(0, max_mot, 0.01))

# prediction for motiles
pred_mot <- data.frame(
    # all motiles count
    all_mots = mot_seq$all_mot,
    pred_prop_mots = stats::predict(
        # model object here
        model,
        mot_seq,
        type = "response",
        se.fit = TRUE
    )
)

# add 95% CI's
pred_df <- pred_mot %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        # fitted value
        pred_prop_mots = pred_prop_mots.fit,
        # lower CI bound
        lower = pred_prop_mots - 1.96 * pred_prop_mots.se.fit,
        # upper CI bound
        upper = pred_prop_mots + 1.96 * pred_prop_mots.se.fit
    ) %>%
    # keep only columns of use
    dplyr::select(all_mots, pred_prop_mots, lower, upper)

mot_plot <- ggplot2::ggplot() +
    geom_point(
        data = df_2002_onwards, aes(x = all_mot, y = prop_lep_mot),
        shape = 21,
        colour = "black",
        fill = "red2",
        alpha = 0.1,
        size = 3,
        position = position_jitter()
    ) +
    geom_ribbon(
        data = pred_df, aes(x = all_mots, ymin = lower, ymax = upper),
        fill = "grey80"
    ) +
    geom_line(
        data = pred_df, aes(x = all_mots, y = pred_prop_mots),
        linewidth = 1.2
    ) +
    theme_better() +
    labs(x = "Number of All Motiles", y = "Proportion of L. salmonis") +
    scale_size_manual(values = c(3, 1))
ggplot2::ggsave(
    here::here("./figs/count-regressions/motile-model-predictions.png"),
    mot_plot
)

## part 2.3 - predict for 2001 -------------------------------------------------
#' Use the model object to make the prediction for the year 2001

# make prediction for mots
mot_2001_pred <- cbind(
    df_2001,
    pred_prop_mots = stats::predict(
        model,
        data.frame(all_mot = df_2001$all_mot),
        type = "response"
    )
)
mean(mot_2001_pred$pred_prop_mots)

## part 2.4 - make yearly motile averages --------------------------------------

# make a dataframe of yearly averages to reference later on
yearly_avg <- fish_df %>%
    dplyr::select(year, prop_lep_mot) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(avg_prop_mots = mean(prop_lep_mot, na.rm = TRUE))
# replace the 2001 value with the modeled value
yearly_avg$avg_prop_mots[
    which(yearly_avg$year == 2001)
] <- mean(mot_2001_pred$pred_prop_mots)

## part 2.5 - calculate proprotions for the bernoulli draw ---------------------

#' Use the clean scfs data and the motile logistic regression to set up the
#' proportions going into the Bernoulli draw
predicted_df <- fish_df %>%
    # add in the individual level predictions for 2001 via the model predictions
    # note that these are all given by the number of all motiles available
    dplyr::left_join(
        .,
        y = yearly_avg,
        by = "year"
    )

# isolate the part of the data frame that in fact has unidentified motiles
to_impute_rows <- which(!is.na(predicted_df$unid_adult))
for (i in to_impute_rows) {
    lice <- sample(
        x = c(1, 0),
        size = predicted_df[[i, "unid_adult"]], # size is the number to sample
        replace = TRUE,
        prob = c(
            predicted_df[[i, "avg_prop_mots"]], # prob of drawing lep (1)
            (1 - predicted_df[[i, "avg_prop_mots"]]) # prob of drawing cal (0)
        )
    )
    # add leps to the leps column (lep_mot)
    predicted_df$lep_mot[i] <- predicted_df$lep_mot[i] +
        # how many 1s are in the lice vector
        length(which(lice == 1))
    # add cals to the cal column (cal_mot)
    predicted_df$cal_mot[i] <- predicted_df$cal_mot[i] +
        # how many 0s are in the lice vector
        length(which(lice == 0))
}

readr::write_csv(
    predicted_df,
    here::here("./data/scfs-data/clean/mots-imputed.csv")
)
