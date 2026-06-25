#' DESCRIPTION: We need to come up with a count of the number of L. salmonis
#' per wild fish, so we estimate the number of lice on wild juvenile salmon
#' by accounting for the number of motile, chalimus, and copepodite lice
#' sepeartely, using year-level predictions.
#' DATE: 20 January 2026
#' AUTHOR: Cole

# load things in ---------------------------------------------------------------
source(here::here("./R/functions/theme_better.R"))
source(here::here("./R/functions/global.R"))

library(magrittr)
library(ggplot2)

#' part 1 - for years where copepodites were mostly speciated, 2005 - present,
#' estimate the number of unidentified copepodites that were actually
#' L. salmonis using the empirical proportion of L. salmonis among the
#' speciated copepodites


#' part 2 - for the years where motiles were mostly speciated, 2002 - present,
#' estimate the number of unidentified motiles that were actually L.
#' salmonis using the empirical proportion of L. salmonis among the speciated
#' motiles

#' part 3 - for the years where copepodites (2002-2004) and motiles (2001)
#' were counted but never speciated, we estimate the number of L. salmonis in
#' these life stages using predicted L. salmonis proportions from simple
#' logistic regressions fit to the speciated years' copepodite and motile
#' data

#' part 4 - we calculate a mean proportion of L. salmonis in a given year/stage
#' and apply that to all individual observations via a random draw from a
#' Bernoulli distribution, with a weighted probability according to our mean
#' proportion. That is, for each unidentified adult louse, to decide if that
#'  louse was L. salmonis, we drew from a Bernoulli distribution, where the
#' probability of drawing a 1 (and therefore counting that louse as an
#' L. salmonis louse) was equal to the average predicted proportion of L.
#' salmonis

#' part 5 - we estimate the number of L. salmonis chalimus-stage lice
#' by applying the average of the L. salmonis proportions for copepodites and
#' for motiles. Since copepodites were counted as chalimus in 2001, we
#' estimated the L. salmonis proportion for chalimus in that year using
#' only the motile L. salmonis proportion.

# set up fish data -------------------------------------------------------------

fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)

# part 1 -----------------------------------------------------------------------
#' Prep data on copepeodites to model
df_2005_onwards <- fish_df %>%
    dplyr::filter(year > 2004) %>%
    dplyr::filter(all_cope > 0)

df_2002_2004 <- fish_df %>%
    dplyr::filter(year > 2001 & year < 2005)

## part 1.1 - regression on the copes ------------------------------------------

model <- glm(cbind(lep_cope, all_sp_cope - lep_cope) ~ all_cope,
    # binomial family
    family = binomial(link = "logit"),
    data = df_2005_onwards
)

# save object as nice neat file
coefs <- broom::tidy(model)
fitted_vals <- broom::augment(model)
model_vals <- broom::glance(model)

## part 1.2 - prediction -------------------------------------------------------
#' Take in the fitted model object and make predictions for the values that
#' the model actually covers out to the maximum count of number of copepodites

# find the maximum number of copepodites
max_cope <- max(df_2005_onwards$all_cope)

# make sequence of values ot predict on
cope_seq <- data.frame(all_cope = seq(0, max_cope, 0.01))

# prediction for copepodites
pred_cope <- data.frame(
    # all copepodites count
    all_cope = cope_seq$all_cope,
    pred_prop = stats::predict(
        # model object here
        model,
        cope_seq,
        type = "response",
        se.fit = TRUE
    )
)

# add 95% CI's
pred_df <- pred_cope %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
        # fitted value
        pred_prop = pred_prop.fit,
        # lower CI bound
        lower = pred_prop - 1.96 * pred_prop.se.fit,
        # upper CI bound
        upper = pred_prop + 1.96 * pred_prop.se.fit
    ) %>%
    # keep only columns of use
    dplyr::select(all_cope, pred_prop, lower, upper)

cope_plot <- ggplot2::ggplot() +
    geom_point(
        data = df_2005_onwards, aes(x = all_cope, y = prop_lep_cope),
        shape = 21,
        colour = "black",
        fill = "blue2",
        alpha = 0.1,
        size = 3,
        position = position_jitter()
    ) +
    geom_ribbon(
        data = pred_df, aes(x = all_cope, ymin = lower, ymax = upper),
        fill = "grey80"
    ) +
    geom_line(
        data = pred_df, aes(x = all_cope, y = pred_prop),
        linewidth = 1.2
    ) +
    theme_better() +
    labs(x = "Number of All Copepodites", y = "Proportion of L. salmonis") +
    scale_size_manual(values = c(3, 1))

ggplot2::ggsave(
    here::here("./figs/count-regressions/cope-model-predictions.png"),
    cope_plot
)

## part 1.3 - predict for 2002-2004 --------------------------------------------
cope_2002_2004_pred <- data.frame(
    df_2002_2004,
    # predicted column
    pred_prop = stats::predict(
        model,
        data.frame(all_cope = df_2002_2004$all_cope),
        type = "response"
    )
)

## part 1.4 - make yearly motile averages --------------------------------------

# make a dataframe of yearly averages to reference later on
yearly_avg <- fish_df %>%
    dplyr::select(year, prop_lep_cope) %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(avg_prop_copes = mean(prop_lep_cope, na.rm = TRUE))
# replace the 2001 value with the modeled value
cope_2002_2004_pred <- cope_2002_2004_pred %>%
    dplyr::group_by(year) %>%
    dplyr::summarize(x = mean(pred_prop)) %>% # just calling it x cos lazy lol
    dplyr::select(x)

yearly_avg$avg_prop_copes[
    which(yearly_avg$year %in% c(2002:2004))
] <- cope_2002_2004_pred$x

## part 1.5 - calculate proprotions for the bernoulli draw ---------------------

#' Use the clean scfs data and the motile logistic regression to set up the
#' proportions going into the Bernoulli draw
predicted_df <- fish_df %>%
    # add in the individual level predictions for 2002-2004 via the model
    # predictions. note that these are all given by the number of all copes
    # available
    dplyr::left_join(
        .,
        y = yearly_avg,
        by = "year"
    )

# isolate the part of the data frame that in fact has unidentified copes
to_impute_rows <- which(!is.na(predicted_df$unid_cope))
for (i in to_impute_rows) {
    lice <- sample(
        x = c(1, 0),
        size = predicted_df[[i, "unid_cope"]], # size is the number to sample
        replace = TRUE,
        prob = c(
            predicted_df[[i, "avg_prop_copes"]], # probability of drawing lep (1)
            (1 - predicted_df[[i, "avg_prop_copes"]]) # probability of drawing cal (0)
        )
    )
    # add copes to the leps column (lep_cope)
    predicted_df$lep_cope[i] <- predicted_df$lep_cope[i] +
        # how many 1s are in the lice vector
        length(which(lice == 1))
    # add copes to the cal column (cal_mot)
    predicted_df$cal_cope[i] <- predicted_df$cal_cope[i] +
        # how many 0s are in the lice vector
        length(which(lice == 0))
}

readr::write_csv(
    predicted_df,
    here::here("./data/scfs-data/clean/copes-imputed.csv")
)
