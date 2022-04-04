########## 
##########
# Set up functions to do data cleaning tasks
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-03-11
##########
##########

# functions ====================================================================

`%notin%` = Negate(`%in%`)

bind_map_data = function(raw_df, sampled) {

    raw_df_cleaned = raw_df %>% 

        # filter to only area of interest
        dplyr::filter(
            `Finfish Aquaculture Reporting Zone` == "Broughton Archipelago"
        ) %>%

        # keep names we want to group by
        dplyr::select(
            `Site Common Name`, Longitude, Latitude,
            `Average L. salmonis motiles per fish`
        ) %>%

        # rename columns so they're consistent with how they're plotted
        dplyr::rename(
            farm = `Site Common Name`,
            lat = Latitude,
            long = Longitude,
            avg_leps = `Average L. salmonis motiles per fish`
        ) %>%

        # need to make farm into a factor so we can group by it
        dplyr::mutate(
            farm = as.factor(farm)
        ) %>%

        # group by to get the mean at the level we want
        dplyr::group_by(
            farm, lat, long
        ) %>%

        # summarize to get the mean across the groups 
        dplyr::summarize(
            mean_leps = mean(avg_leps, na.rm = TRUE)
        ) %>%

        # filter to get rid of the confusing wrong latitude values 
        dplyr::filter(
            lat > 0
        ) %>%

        # ungroup because the ifelse needs a character not a factor
        dplyr::ungroup()

    ### add in a column denoting sampled versus unsampled farms 
    raw_df_cleaned_sampled = raw_df_cleaned %>%

        # make new column 
        dplyr::mutate(
            sampled =
            ifelse(farm_data_sum$farm %in% sampled,
                    "sampled", 
                    "unsampled"
            )
        )

    ### make separate object with only the sampled farms
    sampled_farms_df = raw_df_cleaned_sampled %>% 
        dplyr::filter(
            sampled == "sampled"
        )
}

standardize_names = function(df) {

    # get current set of names
    current_names = names(df)

    # loop through, pull the name out, change "." to "_"
    for(name in seq_len(length(current_names))) {
        current_names[name] = gsub("\\.", "_", current_names[name])
    }

    # check for any upper case letters and make those lower_case
    current_names = tolower(current_names)

    # standardize reference to cals or leps 
    for(name in seq_len(length(current_names))) {
        current_names[name] = gsub("caligus", "cal", current_names[name])
        current_names[name] = gsub("cals", "cal", current_names[name])
        current_names[name] = gsub("leps", "lep", current_names[name])
    }

    # rename the dataframe
    names(df) = current_names

    #return dataframe renamed 
    return(df)
}