########## 
##########
# All functions related to comparing the different scenarios for estimating the
# number of lice on wild fish
##########
##########
# AUTHOR: Cole B. Brookson
# DATE OF CREATION: 2022-08-13
##########
##########

options(dplyr.summarise.inform = FALSE)

# scenario 1 ===================================================================

#' Through the years of this survey, due to slight differences in sampling 
#' levels, there were variable numbers of lice in all stages of interest 
#' (copepodids, chalimus, and motiles) that were identified to species. For 
#' years in which copepodites were mostly speciated (i.e., 2005 - present), we 
#' estimated the number of unidentified copepodites that were actually 
#' L. salmonis using the empirical proportion of L. salmonis among the speciated 
#' copepodites. Similarly, for years in which motiles were mostly speciated 
#' (i.e., 2002 - present), we estimated the number of unidentified motiles that 
#' were actually L. salmonis using the empirical proportion of L. salmonis among 
#' the speciated motiles. 
#' 
#' For years in which copepodites (i.e., 2002 - 2004) and motiles (i.e., 2001) 
#' were counted but never speciated, we estimated the number of L. salmonis in 
#' these life stages using predicted L. salmonis proportions from simple
#' logistic regressions fit to the speciated years’ copepodite and motile data.
#' 
#' We estimated the number of L. salmonis chalimus-staged lice by applying the 
#' average of the L. salmonis proportions for copepodites and for motiles. 
#' Since copepodites were counted as chalimus in 2001, we estimated the L. 
#' salmonis proportion for chalimus in that year using only the motile L. 
#' salmonis proportion.
#' 
#' -----------
#'     YEAR     |   COPEPODITE   |   MOTILE    |
#'     2001     |   predicted    |  predicted  |
#'  2002-2004   |   predicted    |  empirical  |
#' 2005-present |   empirical    |  empirical  |
# ==============================================================================

