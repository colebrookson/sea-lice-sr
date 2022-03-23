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