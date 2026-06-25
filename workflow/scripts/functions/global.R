#' Sum Values with NA Handling
#'
#' Computes the sum of the input values, ignoring NA values by default.
#' If all input values are NA, returns NA.
#'
#' @param ... Numeric values to be summed. Can be individual numbers or
#'   vectors.
#' @param na.rm Logical; should missing values (NA) be removed?
#'   Defaults to \code{TRUE}.
#'
#' @return A numeric value representing the sum of the input values, or NA
#'   if all values are NA.
#'
#' @examples
#' my_sum(1, 2, 3)
#' my_sum(1, NA, 3)
#' my_sum(NA, NA)
#'
#' @export
my_sum <- function(..., na.rm = TRUE) {
    values <- c(...) # Collect the input values
    if (all(is.na(values))) {
        return(NA) # Return NA if all values are NA
    } else {
        return(sum(values, na.rm = na.rm)) # Sum ignoring NA if not all are NA
    }
}

#' Standard Deviation with NA Handling
#'
#' Computes the standard deviation of the input values, ignoring NA values by
#' default. If all input values are NA, returns NA.
#'
#' @param ... Numeric values to be used in the calculation. Can be individual
#' numbers or vectors.
#' @param na.rm Logical; should missing values (NA) be removed?
#'   Defaults to \code{TRUE}.
#'
#' @return A numeric value representing the standard deviation of the input
#'  values, or NA if all values are NA.
#'
#' @examples
#' my_sd(1, 2, 3)
#' my_sd(1, NA, 3)
#' my_sd(NA, NA)
#'
#' @export
my_sd <- function(..., na.rm = TRUE) {
    values <- c(...) # Collect the input values
    if (all(is.na(values))) {
        return(NA) # Return NA if all values are NA
    } else {
        return(sd(values, na.rm = na.rm)) # SD ignoring NA if not all are NA
    }
}

#' Mean Value with NA Handling
#'
#' Computes the mean of the input values, ignoring NA values by
#'   default. If all input values are NA, returns NA.
#'
#' @param ... Numeric values to be averaged. Can be individual numbers or
#'   vectors.
#' @param na.rm Logical; should missing values (NA) be removed?
#'   Defaults to \code{TRUE}.
#'
#' @return A numeric value representing the mean of the input values, or NA
#'   if all values are NA.
#'
#' @examples
#' my_mean(1, 2, 3)
#' my_mean(1, NA, 3)
#' my_mean(NA, NA)
#'
#' @export
my_mean <- function(..., na.rm = TRUE) {
    values <- c(...)
    if (all(is.na(values))) {
        return(NA)
    } else {
        return(mean(values, na.rm = na.rm))
    }
}

#' Minimum Value with NA Handling
#'
#' Computes the minimum of the input values, ignoring NA values by
#'   default. If all input values are NA, returns NA.
#'
#' @param ... Numeric values to be compared. Can be individual numbers or
#'   vectors.
#' @param na.rm Logical; should missing values (NA) be removed?
#'   Defaults to \code{TRUE}.
#'
#' @return A numeric value representing the minimum of the input values, or
#'   NA if all values are NA.
#'
#' @examples
#' my_min(1, 2, 3)
#' my_min(1, NA, 3)
#' my_min(NA, NA)
#'
#' @export
my_min <- function(..., na.rm = TRUE) {
    values <- c(...)
    if (all(is.na(values))) {
        return(NA)
    } else {
        return(min(values, na.rm = na.rm))
    }
}

#' Maximum Value with NA Handling
#'
#' Computes the maximum of the input values, ignoring NA values by
#'   default. If all input values are NA, returns NA.
#'
#' @param ... Numeric values to be compared. Can be individual numbers or
#'   vectors.
#' @param na.rm Logical; should missing values (NA) be removed?
#'   Defaults to \code{TRUE}.
#'
#' @return A numeric value representing the maximum of the input values, or
#'   NA if all values are NA.
#'
#' @examples
#' my_max(1, 2, 3)
#' my_max(1, NA, 3)
#' my_max(NA, NA)
#'
#' @export
my_max <- function(..., na.rm = TRUE) {
    values <- c(...)
    if (all(is.na(values))) {
        return(NA)
    } else {
        return(max(values, na.rm = na.rm))
    }
}

prop_zero <- function(x) mean(x == 0)

`%notin%` <- Negate(`%in%`)


standardize_names <- function(df) {
    # get current set of names
    current_names <- names(df)

    # loop through, pull the name out, change "." to "_"
    for (name in seq_len(length(current_names))) {
        current_names[name] <- gsub("\\.", "_", current_names[name])
    }

    # check for any upper case letters and make those lower_case
    current_names <- tolower(current_names)

    # standardize reference to cals or leps
    for (name in seq_len(length(current_names))) {
        current_names[name] <- gsub("caligus", "cal", current_names[name])
        current_names[name] <- gsub("cals", "cal", current_names[name])
        current_names[name] <- gsub("leps", "lep", current_names[name])
    }

    # rename the dataframe
    names(df) <- current_names

    # return dataframe renamed
    return(df)
}
