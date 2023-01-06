# --------------------------------------------------------------------------------------------------------------------
# Functions to calculate carelessness scores acording to the five considered methods, and a function to calculate recall curves
#
# Last changed on December 12, 2022
# --------------------------------------------------------------------------------------------------------------------


# calculate psychometric synonyms
# x is data matrix, mincorr is minimum correlation for assigning pairs. 
# Default is 0.6, which is default value in implementation in careless package
synonym <- function(x, mincorr = 0.6)
{
  # in case no synonyms can be computed (not enough pairs), return NAs
  tryCatch(careless::psychsyn(x = x, critval = mincorr, anto = FALSE, 
                              diag = FALSE, resample_na = TRUE),
           error = function(...){
             rep(NA_real_, nrow(x))
           })
}


# calculate psychometric antonyms
# x is data matrix, mincorr is minimum correlation for assigning pairs. 
# Default is -0.6, which is default value in implementation in careless package
antonym <- function(x, mincorr = -0.6)
{
  # in case no antonyms can be computed (not enough pairs), return NAs
  tryCatch(careless::psychant(x = x, critval = mincorr, diag = FALSE),
           error = function(...){
             rep(NA_real_, nrow(x))
           })
}


# calculate personal reliability (also called even-odd consistency)
# x is a data matrix, 'constructs' a vector of integers specifying the length of each construct in the dataset
reliability <- function(x = x, constructs)
{
  # omit uninformative warning and
  # multiply by (-1) so that low values are indicative of carelessness
  suppressWarnings(
    -careless::evenodd(x = x, factors = constructs, diag = FALSE)
  )
}


# calculate squared Mahalanobis distance
# x is a data matrix
mahalanobis_sq <- function(x)
{
  careless::mahad(x = x, plot = FALSE)
}


# calculate autoencoder reconstruction errors as in paper
# x is a matrix holding an autoencoder reconstruction error in each cell 
RE <- function(x)
{
  # average over the individual item reconstruction errors 
  return(rowMeans(x))
} # FUN


#' calculate recall curve
#' @param x is a numeric vector of carelessness scores for each participant
#' @param contam is a vector of indices in \code{x} that are associated with careless respondents
#' @param right If TRUE, then we expect scores of careless respondents to be in right tail (higher values of \code{x} are indicative of carelessness). If FALSE, then lower values are indicative of carelessness 
recall <- function(x, contam, right = TRUE)
{
  xord <- order(x = x, decreasing = right)
  return(
    sapply(seq_along(xord), function(i) mean(contam %in% xord[1:i]))
  )
} # FUN
