#' DESCRIPTION: We have a number of different "scenarios" that we need to ensure
#' are represented in the data. We will describe them here: 
#' 
#' # scenario 1 ================================================================

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
#' 
#' Within this approach, once we get our proportion, we use that proportion to 
#' draw from a Bernoulli distribution to decide if each of the unidentified 
#' motiles or copes are L. salmonis or not. We compare two ways of doing this. 
#' 
#' First, We calculated the mean proportion in a given year/stage and applied 
#' that to all individual observations via random sampling with a set 
#' probability according to our mean proportion. To illustrate this more 
#' clearly, for 2001 motiles, the average predicted portion of L. salmonis is 
#' 0.639. For each unidentified adult louse, to decide if that louse was 
#' L. salmonis, we drew from a Bernoulli distribution, where the probability 
#' of drawing a 1 (and therefore counting that louse as an L. salmonis louse) 
#' was equal to 0.639. In circumstances where there were >1 unidentified adult 
#' lice, this draw was repeated for each individual louse. This is more 
#' biologically realistic than ascribing 0.639 lice to a fish. 
#' 
#' Second, we use the predicted proportion given by the fitted model, to draw
#' our Bernoulli sample on the level of each fish. That is, for each row in the
#' data set, a different proportion is used to draw from the distribution. 
#' 
#' Comparing these two approaches is the difference between assumptions. The 
#' first option assumes that the proportion of L. salmonis on juvenile salmon 
#' is due primarily to the number of motiles overall on that juvenile. This 
#' means that in a well-mixed pop'n, the proportion changes as the number of 
#' motiles changes. The second option assumes that the proportion on juveniles
#' is primarily driven by changes through years external to the individual 
#' infection patterns of the fish. 
# ==============================================================================