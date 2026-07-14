#' DESCRIPTION: we need a year by stage table of l. salmonis proportions to 
#' do the overall imputation on
#' AUTHOR: Cole Brookson

# load things in ---------------------------------------------------------------
source(here::here("./workflow/scripts/functions/theme_better.R"))
source(here::here("./workflow/scripts/functions/global.R"))
source(here::here("./workflow/scripts/functions/wild_lice_functions.R"))
library(magrittr)

fish_df <- readr::read_csv(
    here::here("./data/scfs-data/clean/standardized-fish-data.csv")
)

POOLED <- TRUE # see empirical_prop() docs 

# get empirical proportions from speciated years -------------------------------
emp_props <- fish_df %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarize(
        p_mot = dplyr::if_else(
            dplyr::first(mot_speciated), 
            empirical_prop(lep_mot_obs, sp_mot, POOLED), 
            NA_real_
        ), 
        p_cope = dplyr::if_else(
            dplyr::first(cope_speciated), 
            empirical_prop(lep_cope_obs, sp_cope, POOLED), 
            NA_real_
        ),
        .groups = "drop"
    )

# logistic regression fit to speciated years -----------------------------------
#' we just want the response to be the speciated lice only and the predictor is 
#' the total for that stage, including the unid's 
mot_fit_df <- fish_df %>% 
    dplyr::filter(mot_speciated, all_mot > 0)
cope_fit_df <- fish_df %>% 
    dplyr::filter(cope_speciated, all_cope > 0)

mot_model <- stats::glm(
    cbind(lep_mot_obs, cal_mot_obs) ~ all_lice, 
    family = stats::binomial(link = "logit"), 
    data = mot_fit_df
)
cope_model <- stats::glm(
    cbind(lep_cope_obs, cal_cope_obs) ~ all_lice, 
    family = stats::binomial(link = "logit"), 
    data = cope_fit_df
)

# predict onto the unspeciated years -------------------------------------------
#' we want to average the indiv predictions on the fish that carry lice of 
#' that stage, i.e. the same population the model was fit on, an dthe only fish 
#' that the proportion can be applied to. 
mean_pred <- function(model, newdata) {
    if(nrow(newdata) == 0) {
        return(NA_real_)
    }
    mean(stats::predict(model, newdata = newdata, type = "response"))
}

# predict onto the 2001 for the mots 
p_mot_2001 <- mean_pred(
    mot_model, 
    fish_df %>% dplyr::filter(year == 2001, all_mot > 0)
)

# copes for '02 to '04
p_cope_0204 <- fish_df %>% 
    dplyr::filter(year %in% c(2002:2004), all_cope > 0) %>% 
    dplyr::group_by(year) %>% 
    dplyr::group_modify(\(d, k) tibble::tibble(
        p_cope_model = mean(stats::predict(cope_model, newdata = d, 
        type = "response"))
    )) %>% 
    dplyr::ungroup()

# put proportions together -----------------------------------------------------
#' now, chalimus is the average of the lep proportions for copepodites and for 
#' mots, but note that in 2001 chalimus in that year uses only mots as the 
#' proportion since copes were counted as chalimus
props <- emp_props %>% 
    dplyr::left_join(p_cope_0204, by = "year") %>% 
    dplyr::mutate(
        p_mot = dplyr::if_else(year == 2001, p_mot_2001, p_mot), 
        p_cope = dplyr::coalesce(p_cope, p_cope_model), 
        p_chal = dplyr::if_else(
            year == 2001, 
            p_mot, 
            (p_mot + p_cope) / 2
        )
    ) %>% 
    dplyr::select(year, p_mot, p_cope, p_chal)

# each year needs motile and chalimus proportion 
stopifnot(
    !any(is.na(props$p_mot)),
    !any(is.na(props$p_chal)),
    !any(is.na(props$p_cope[props$year != 2001]))
)
readr::write_csv(props, 
    here::here("./data/scfs-data/clean/lep-proportions.csv")
)



ggplot2::ggsave(
    here::here("./figs/count-regressions/motile-model-predictions.png"),
    plot_stage(mot_fit_df, mot_model, "all_lice", "lep_mot_obs", "sp_mot",
               "Total lice on fish", "red2")
)