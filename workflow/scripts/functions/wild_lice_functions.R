#' DESCRIPTION: These are some easy helper functions for the species-level
#' imputation that we have to do on the wild lice. Essentially, the raw data 
#' can't really tell whether a blank is a zero or missing (see SCFS data, 
#' "We also do not distinguish zeroes from missing data for the louse 
#' information"). So we do NOT infer it from the data. 
#' AUTHOR: Cole Brookson

#' lice_protocol
#' 
#' @description taken from the SCFS metadata, we look to describe stages as 
#' counted or speciated in a given year 
#' @param years the years of the data
#' 
#' @return a tibble showing the stage/counted information for given years 
lice_protocol <- function(years) {
    tibble::tibble(year = years) %>% 
    dplyr::mutate(
        cope_counted = year >= 2002,
        cope_speciated = year >= 2005,
        mot_counted = TRUE,
        mot_speciated = year >= 2002,
        chal_counted = TRUE,
        chal_speciated = FALSE
    )
} 

# ---- column groups -----------------------------------------------------------
COPE_COLS <- c("lep_cope", "cal_cope", "unid_cope")
CHAL_COLS <- c("chala", "chalb", "chal_unid")
MOT_COLS  <- c(
    "lep_pamale", "lep_pafemale", "lep_male", "lep_nongravid", "lep_gravid",
    "cal_mot", "cal_gravid", "unid_adult", "unid_pa"
)

#' fill_counted_stages
#' 
#' @description Within a stage that was actually counted, if it's blank the 
#' counter examined the fish, found none of that category, so it's a zero. e.g. 
#' in 2002-2004, lep_cope = 0, because no copepodites were identified as leps 
#' cos no one looked. These lice are all in the `unid_cope` column. So, for a 
#' stage that wasn't counted, each column stays NA, and every derived total 
#' stays NA. 
#' @param df the dataframe that we're working with
fill_counted_stages <- function(df) {
    df %>% 
    dplyr::mutate(
        dplyr::across(
            dplyr::any_of(COPE_COLS), 
            \(x) dplyr::if_else(cope_counted & is.na(x), 0, x)
        ), 
        dplyr::across(
            dplyr::any_of(MOT_COLS), 
            \(x) dplyr::if_else(mot_counted & is.na(x), 0, x)
        ), 
        dplyr::across(
            dplyr::any_of(CHAL_COLS), 
            \(x) dplyr::if_else(chal_counted & is.na(x), 0, x)
        )
    )
}

#' draw_leps 
#' 
#' @description for each unidentified louse, draw Bernoulli(p) to decide if it
#' is an L. salmonis. A sum of `n` iid Bernoulli draws is just Binomial(n,p), 
#' so we do rbinom(size = n, prob = p) to vectorize the bernoulli draw. To keep
#' the size NA (if the stage isn't counted), NA out. size 0 -> 0 out, no draw. 
#' prob NA with size > 0 -> doesn't r87n. 
#' @param size the number of draws 
#' @param prob the probability needed 
draw_leps <- function(size, prob) { 
    out <- rep(NA_real_, length(size))

    is_zero <- !is.na(size) & size == 0
    out[is_zero] <- 0

    needs <- !is.na(size) & size == 0
    if (any(needs & is.na(prob))) {
        stop(
            "draw_leps(): ", sum(nees & is.na(prob)),
            " fish have unidentified lice but no L. salmonis proportion. ",
            "Check the proportion table."
        )
    }
    out[needs] <- stats::rbinom(
        sum(needs), 
        size = size[needs], 
        prob = prob[needs]
    )

    return(out)
}

#' empirical_prop
#' 
#' @description we need a pooled proportion of: total lep / total speciated, 
#' across all fish in the year. We can set pooled = FALSE to take the mean of 
#' ratios, but this gives a fish carrying 1 louse the same weight as a fish 
#' carrying 40 lice, so the mean-of-ratios is biased downwards. 
#' @param lep
#' @param spec 
#' @param pooled = TRUE
empirical_prop <- function(lep, spec, pooled = TRUE) { 
    if(pooled) { 
        denom <- my_sum(spec)
        if(denom == 0) reutnr(NA_real_)
        my_sum(lep) / denom 
    } else { 
        my_mean(dplyr::if_else(
            spec > 0, lep / spec, NA_real_   
            ))
    }
}