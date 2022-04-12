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

join_marty_bati_data = function(marty_df, bati_df, farm_map_nums) {

    # get rid of date column in bati_df 
    bati_df = bati_df %>% 
        dplyr::select(-date)

    # make sure all farm names match up - DEFAULT TO MARTY NAMES
    bati_names = sort(unique(bati_df$farm))
    marty_names = sort(unique(marty_df$farm_name))

    bati_df_renamed = bati_df %>% 

        # make sure the levels of the bati data farm names match 
        dplyr::mutate(
            farm_name = forcats::fct_recode(farm, 
                "Arrow Pass" = "Arrow Passage",
                "Humphrey Rock" = "Humphrey Rocks",
                "Larson Island" = "Larsen Island",
                "Midsummer" = "Midsummer Island",
                "Sargeaunt Pass" = "Sargeaunt Passage",
                "Swanson" = "Swanson Island",
                "Doctors Islets" = "Doctor Islets",
            )
        ) %>% 

        # get rid of old farm names 
        dplyr::select(-farm)

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
            lep_av = lep_av_mot
        )

    # defer to BATI data, so only keep Marty data where BATI is not present
    marty_df_deferred = marty_df_cleaned %>% 

        # create new column to denote if year/farm observation is in BATI df
        dplyr::rowwise() %>% 
        dplyr::mutate(
            keep = dplyr::case_when(
                (farm_name %in% # check for farm name
                    unique(bati_df_renamed$farm_name)) &
                (year %in% # chcke also for year
                    unique(bati_df_renamed$year))         ~ "bati",
                TRUE                                      ~ "marty"
            )
        ) %>% 

        # now filter out the "bati" obs as those will be provided by that df
        dplyr::filter(
            keep == "marty"
        ) %>% 

        # get rid of the keep column since we've used it 
        dplyr::select(-keep)

    # check the two df's are the name in terms of their column names 
    if (!identical(sort(names(marty_df_deferred)), 
                   sort(names(bati_df_renamed)))) {

        stop("column names not the same!")
    }

    # order the two dataframes the same so they can be bound together 
    col_order = c(
        "farm_name", "year", "month", "inventory", "lep_av", 
        "lep_tot", "cal_av", "cal_tot"
    )
    reorder_marty_df = marty_df_deferred[, col_order]
    reorder_bati_df = bati_df_renamed[, col_order]

    # bind dataframes 
    bound_df = data.frame(
        rbind(reorder_marty_df, reorder_bati_df)
    )

    # take the extra new farms out of farm map names df 
    farm_map_names_timmed = farm_map_nums %>% 

        # filter 
        dplyr::filter(
            farm_name %notin% c("Wa-kwa", "Tsa-ya")
        )

    # make sure farm names match between the bound_df and the farm map names 
    if (!identical(sort(unique(bound_df$farm_name)), 
                   sort(unique(farm_map_names_timmed$farm_name)))) {
        stop("column names not the same!")
    }


    # return 
    return(bound_df)

}

bind_map_data = function(
    raw_marty_data, 
    farm_locations_df,
    dfo_open_data,
    sampled_farms) {

    # clean/standardize marty data
    marty_df_trimmed = raw_marty_data %>% 

        # get only the columns I want 
        dplyr::select(
            Year, `Farm # on  Map`, Month, `Motile L.s./ fish`, `# fish`
        ) %>% 

        # rename 
        dplyr::rename(
            year = Year,
            farm_num = `Farm # on  Map`,
            month = Month,
            avg_leps = `Motile L.s./ fish`,
            inventory = `# fish`
        ) %>% 

        # trim down data to exclude unneeded calculations at the bottom
        dplyr::slice(
            -(2507:nrow(raw_marty_data))
        )

    # join farm locations df with marty data to get locations 
    marty_locs = dplyr::left_join(
        marty_df_trimmed,
        farm_locations_df,
        by = "farm_num"
    )

    # put mean leps in dataframe 
    marty_summarized = marty_locs %>% 

        # group by to get the mean at the level we want
        dplyr::group_by(
            farm_name, lat, long
        ) %>%

        # summarize to get the mean across the groups 
        dplyr::summarize(
            mean_leps = mean(avg_leps, na.rm = TRUE)
        ) %>%

        # ungroup because the ifelse needs a character not a factor
        dplyr::ungroup()

    # clean/standardize dfo open data
    dfo_open_data_trimmed = dfo_open_data %>% 

        # keep only the Broughton area observations
        dplyr::filter(
            `Finfish Aquaculture Reporting Zone` == "Broughton Archipelago"
        ) %>% 

        # keep only specific columns
        dplyr::select(
            Year, Month, `Site Common Name`, Latitude, 
            Longitude, `Average L. salmonis motiles per fish`
        ) %>% 

        # rename to standard 
        dplyr::rename(
            year = Year, 
            month = Month, 
            farm_name = `Site Common Name`,
            lat = Latitude,
            long = Longitude,
            avg_leps = `Average L. salmonis motiles per fish`
        ) %>% 

        # get rid of the two separate locations for Tsa-ya
        dplyr::mutate(
            lat = ifelse(farm_name == "Tsa-ya" & lat != 50.61330, 
                            50.61330, lat),
            long = ifelse(farm_name == "Tsa-ya" & long != -126.33073, 
                            -126.33073, long)
        ) %>% 

        # flip the wrong lat long values
        dplyr::rowwise() %>% 
        dplyr::mutate(
            fixed_lat = ifelse(lat > 0, lat, long),
            fixed_long = ifelse(long < 0, long, lat)
        ) %>%
        dplyr::select(
            -lat, -long
        ) %>% 
        dplyr::rename(
            lat = fixed_lat,
            long = fixed_long
        ) %>% 

        # group by to get the mean at the level we want
        dplyr::group_by(
            farm_name, lat, long
        ) %>% 

        # summarize to get the mean across the groups 
        dplyr::summarize(
            mean_leps = mean(avg_leps, na.rm = TRUE)
        ) %>%

        # recode the levels of the names 
        dplyr::mutate(farm_name = factor(farm_name)) %>% 
        dplyr::mutate(
            farm_name = forcats::fct_recode(farm_name, 
                "Doctors Islets" = "Doctor Islets",
                "Larson Island" = "Larsen Island",
                "Whlis Bay" = "Wehlis Bay"
            )
        ) %>% 

        # ungroup because the ifelse needs a character not a factor
        dplyr::ungroup()

    # pull in the other glacier location and add in the more recent farms 
    map_df = rbind(

        # marty data pulled in as is
        marty_summarized,

        # filter dfo data to only the ones not in marty_summarized
        dfo_open_data_trimmed %>% 
            # only keep the rows not in the marty data
            dplyr::filter(farm_name %notin% marty_summarized$farm_name &
                            farm_name != "Glacier Falls"),

        # filter the farm location one to just the glacier falls 
        farm_locations_df %>% 
                dplyr::select(-farm_num) %>% 
                dplyr::filter(farm_name == "Glacier Falls (2)") %>% 
                dplyr::mutate(mean_leps = (marty_summarized %>% 
                    dplyr::filter(farm_name == "Glacier Falls (1)"))$mean_leps)
    )

    # add in columns for numbers and sampled columns
    map_df_sampled = map_df %>%

        # make new column 
        dplyr::mutate(
            sampled =
            ifelse(map_df$farm_name %in% sampled_farms,
                    "sampled",
                    "unsampled"
            )
        ) %>% 

        # add the numbers of the farms according to latitude
        dplyr::arrange(desc(lat)) %>% 
        dplyr::mutate(
            farm_num = seq(1,28,1)
        )

    # add in the different groupings 
    map_df_groupings = map_df_sampled %>% 

        # add value for Knight Tribune Corridor
        dplyr::rowwise() %>% 
        dplyr::mutate(
            ktc = ifelse(farm_name %in% 
                c("Wicklow Point", "NA_14", "NA_12", "Sir Edmund Bay",
                "Burdwood", "NA_7", "Cliff Bay", "Glacier Falls (2)",
                "Glacier Falls (1)", "Humphrey Rock", "Doctors Islets", 
                "Sargeaunt Pass"), "Knight Tribune Corridor", "Broughton"), 
            hump_sarg_doc = ifelse(farm_name %in% 
                c("Sargeaunt Pass", "Doctors Islets", "Humphrey Rock"), 
                "Humphrey-Sargeaunt-Doctors Triangle", "Other")
        )

    # return 
    return(map_df_groupings)

}
