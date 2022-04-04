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
            ifelse(raw_df_cleaned$farm %in% sampled,
                    "sampled", 
                    "unsampled"
            )
        )

    ### make separate object with only the sampled farms
    sampled_farms_df = raw_df_cleaned_sampled %>% 
        dplyr::filter(
            sampled == "sampled"
        )

    return(raw_df_cleaned_sampled)

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

trim_marty_data = function(df) {

    # vec of column names we want to keep 
    names = c(
        "#", "Farm # on  Map", "Month", "Year", "# fish", "Chalimus/ fish",
        "Motile L.s./ fish", "Female L.s./ fish", "Caligus/ fish"
    )

    # vec of new names for these 
    new_names = c(
        "obs_num", "farm_num", "month", "year", "inventory", "chal_fish", 
        "mot_lep_fish", "fem_lep_fish", "cal_fish"
    )

    # trim down data to only include names in the names vector & to get rid of 
    # unneeded calculations at the bottom of the sheet
    df_trimmed = df %>% 
        dplyr::select(all_of(names)) %>% 
        dplyr::slice(-(2507:nrow(df)))

    # rename variables 
    df_trimmed_renamed = df_trimmed %>% 
        dplyr::rename_at(
            vars(names) ~ new_names
        )

    # return 
    return(df_trimmed)

}

farm_names = function(df) {

    # vector of common names of the farm 
    farm_name = c(
    "Simmonds Point", "Whlis Bay", "Maude Island", "Cecil Island",
    "Cypress Harbour", "Sir Edmund Bay", "NA_7", "Cliff Bay", "Glacier Falls",
    "Burdwood", "NA_11", "NA_12", "Wicklow Point", "NA_14", "NA_15",
    "Upper Retreat", "Arrow Pass", "Midsummer", "Potts Bay", "Port Elizabeth",
    "Humphrey Rock", "Sargeaunt Pass", "Doctors Islets", "Swanson", 
    "Larson Island", "Noo-la"
    )

    # vector of the numbers assigned by Gary Mary (2010)
    farm_num = seq(1, 26, 1)

    # make dataframe of the two name types 
    names_df = data.frame(
        text = text_names,
        nums = farm_num
    )

    # bind the df so both name types are present 
    df_both_names = dplyr::left_join(
        df,
        names_df,
        by = "farm_num"
    )

    # return joined
    return(df_both_names)

}