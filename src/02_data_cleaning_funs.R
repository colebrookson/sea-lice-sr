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
        "obs_num", "farm_num", "month", "year", "inventory", "chal_av",
        "lep_av_mot", "lep_av_fem", "cal_av"
    )

    # trim down data to only include names in the names vector & to get rid of 
    # unneeded calculations at the bottom of the sheet
    df_trimmed = df %>% 
        dplyr::select(
            all_of(names)
        ) %>% 
        dplyr::slice(-(2507:nrow(df))
        )

    # rename variables 
    df_trimmed_renamed = df_trimmed %>% 
        dplyr::rename_with(
            ~ new_names[which(names == .x)], .cols = all_of(names)
        )

    # return 
    return(df_trimmed_renamed)

}

farm_names = function(df) {

    # vector of common names of the farm 
    farm_name_vec = c(
    "Simmonds Point", "Whlis Bay", "Maude Island", "Cecil Island",
    "Cypress Harbour", "Sir Edmund Bay", "NA_7", "Cliff Bay", "Glacier Falls", 
    "Burdwood", "NA_12", "Wicklow Point", "NA_14", "NA_15",
    "Upper Retreat", "Arrow Pass", "Midsummer", "Potts Bay", "Port Elizabeth",
    "Humphrey Rock", "Sargeaunt Pass", "Doctors Islets", "Swanson", 
    "Larson Island", "Noo-la"
    )

    # vector of the numbers assigned by Gary Mary (2010)
    farm_num_vec = c(seq(1, 9, 1), seq(11, 26, 1))

    # make dataframe of the two name types 
    names_df = data.frame(
        farm_name = farm_name_vec,
        farm_num = farm_num_vec
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

fix_months = function(df) {

    # use a simple joined dataframe to match across 
    month_names = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug",
                    "Sep", "Oct", "Nov", "Dec")
    month_numbers = seq(1, 12, 1)

    month_df = data.frame(
        month = month_names, 
        num = month_numbers
    )

    # ensure all the month names in the vector above and in the df are the same
    df_names = sort(unique(df$month))
    month_sorted = sort(month_names)

    if (!identical(df_names, month_sorted)) {
        stop("Months in function and months in dataframe do not match!")
    }

    # left join the data 
    df_month_nums = dplyr::left_join(
        df, 
        month_df, 
        by = "month"
    )

    # keep the number of the month and get rid of the text version 
    df_months = df_month_nums %>% 
        dplyr::select(
            -month
        ) %>% 
        dplyr::rename(
            month = num
        )

    # return
    return(df_months)

}

trim_updated_data = function(df) {

    # keep only the Broughton area observations
    df_broughton = df %>% 
        dplyr::filter(
            `Finfish Aquaculture Reporting Zone` == "Broughton Archipelago"
        )

    df_trimmed = df_broughton %>% 
        dplyr::select(
            `_id`, Year, Month, `Site Common Name`, Latitude, Longitude,

        )
}

join_marty_bati_data = function(marty_df, bati_df) {

    # get rid of date column in bati_df 
    bati_df = bati_df %>% 
        dplyr::select(-date)

    # make sure all farm names match up - DEFAULT TO MARTY NAMES
    bati_names = sort(unique(bati_df$farm))
    marty_names = sort(unique(marty_df$farm_name))

    bati_df_renamed = bati_df %>% 
        dplyr::mutate(
            farm = forcats::fct_recode(farm, 
                "Arrow Pass" = "Arrow Passage",
                "Humphrey Rock" = "Humphrey Rocks",
                "Larson Island" = "Larsen Island",
                "Midsummer" = "Midsummer Island",
                "Sargeaunt Pass" = "Sargeaunt Passage",
                "Swanson" = "Swanson Island"
            )
        )

    # make "total" columns in the marty_df
    marty_df_cleaned = marty_df %>%

        # get rid of unnecessary columns
        dplyr::select(
            -obs_num, -farm_num, -chal_av, -lep_av_fem
        ) %>%

        # make the total values for the leps and the cals 
        dplyr::rowwise() %>% 
        dplyr::mutate(
            lep_tot = inventory * lep_av_mot,
            cal_tot = inventory * cal_av
        ) %>% 

        # rename appropriately
        dplyr::rename(
            farm = farm_name,
            lep_av = lep_av_mot
        ) %>%

        # defer to BATI data, so only keep Marty data where BATI is not present
        dplyr::filter(
            year < min(bati_df$year)
        )

    # check the two df's are the name in terms of their column names 
    if (!identical(sort(names(marty_df_cleaned)), 
                   sort(names(bati_df_renamed)))) {

        stop("column names not the same!")
    }

    # order the two dataframes the same so they can be bound together 
    col_order = c(
        "farm", "year", "month", "inventory", "lep_av", 
        "lep_tot", "cal_av", "cal_tot"
    )
    reorder_marty_df = marty_df_cleaned[, col_order]
    reorder_bati_df = bati_df_renamed[, col_order]

    # bind dataframes 
    bound_df = data.frame(
        rbind(reorder_marty_df, reorder_bati_df)
    )

    # return 
    return(bound_df)

}

bind_map_data = function(
    all_farm_data, 
    raw_marty_data, 
    farm_locations_df,
    dfo_open_data) {

    # clean/standardize marty data
    marty_df_trimmed = raw_marty_data %>% 

        # get only the columns I want 
        dplyr::select(
            Year, `Farm # on  Map`, Month, `# Motiles (L.salmonis)`, `# fish`
        ) %>% 

        # rename 
        dplyr::rename(
            year = Year,
            farm_num = `Farm # on  Map`,
            month = Month,
            avg_leps = `# Motiles (L.salmonis)`,
            inventory = `# fish`
        ) %>% 

        # get the farm names 
        farm_names()

    # clean/standardize dfo open data
    dfo_open_data_trimmed = dfo_open_data %>% 

        # keep only the Broughton area observations
        dplyr::filter(
            `Finfish Aquaculture Reporting Zone` == "Broughton Archipelago"
        ) %>% 

        # keep only specific columns
        dplyr::select(
            Year, Month, `Site Common Name`, Latitude, Longitude,
        ) %>% 

        # rename to standard 
        dplyr::rename(
            year = Year, 
            month = Month, 
            farm_name = `Site Common Name`,
            lat = Latitude,
            long = Longitude
        )
    
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