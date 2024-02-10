# ------------------------------------------------------------------------------
# Functions to calculate carelessness scores according for various methods
#
# Created on December 12, 2022, by Max Welz (welz {at} ese.eur.nl)
# Last changed on August 10, 2023, by Andreas Alfons
# ------------------------------------------------------------------------------


## calculate autoencoder mean squared reconstruction errors as in paper
# x is a matrix holding an autoencoder reconstruction error in each cell
# 'x' is data matrix, 'reconstructed' is a matrix of reconstructions,
# 'num_likert' is the number of answer categories
MSRE <- function(x, reconstructed, num_likert) {
  # in case of an error, return NULL
  tryCatch({
    # average over the individual squared item reconstruction errors
    rowMeans(((x - reconstructed) / (num_likert-1))^2)
  }, error = function(e) NULL)
}


## calculate squared Mahalanobis distance
# 'x' is a data matrix
mahalanobis_sq <- function(x) {
  # in case of an error, return NULL
  tryCatch(careless::mahad(x = x, plot = FALSE),
           error = function(e) NULL)
}


## calculate personal reliability (also called even-odd consistency)
# 'x' is a data matrix where the items are ordered according to the scales,
# 'scale_sizes' is a vector of integers specifying the length of each scale
# in the dataset
reliability <- function(x = x, scale_sizes) {
  # in case of an error, return NULL
  tryCatch({
    # omit uninformative warning and multiply by (-1) so that low values are
    # indicative of carelessness
    suppressWarnings(
      -careless::evenodd(x = x, factors = scale_sizes, diag = FALSE)
    )
  }, error = function(e) NULL)
}


## calculate psychometric synonyms
# 'x' is data matrix, 'min_corr' is minimum correlation for item pairs
# (the default is 0.6, which is also the default value in package 'careless'),
# 'min_pairs' is the minimum number of item pairs that should survive the
# correlation screening (the default value 6 is motivated by Goldammer et al.
# (2020), who recommend who recommend using psychometric synonyms only with
# more than 5 item pairs of sufficient correlation)
synonym <- function(x, min_corr = 0.6, min_pairs = 6L) {
  # in case of an error, return NULL
  tryCatch({
    # check whether we have enough item pairs with minimum correlation
    cor_mat <- cor(x)
    cor_ordered <- sort(cor_mat[lower.tri(cor_mat)], decreasing = TRUE)
    if (sum(cor_ordered > min_corr) < min_pairs) {
      # if we don't have enough item pairs,
      min_corr <- mean(cor_ordered[min_pairs + 0:1])
    }
    # compute psychometric synonyms
    careless::psychsyn(x = x, critval = min_corr, anto = FALSE,
                       diag = FALSE, resample_na = TRUE)
  },
  error = function(e) NULL)
}


## intra-individual response variability: standard deviation of responses
## across a set of consecutive item responses for an individual
# 'x' is data matrix
irv <- function(x) {
  # in case of an error, return NULL
  tryCatch(careless::irv(x = x),
           error = function(e) NULL)
}


## lzpoly person-fit statistic
# According to the documentation, aberrant response behavior is (potentially)
# indicated by *small* values of lzpoly
# 'x' is data matrix, 'num_likert' is the number of answer categories
lz <- function(x, num_likert) {
  # in case of an error, return NULL
  tryCatch({
    # responses must be in [0, num_likert-1]
    out <- PerFit::lzpoly(matrix = x - 1, Ncat = num_likert)
    out$PFscores$PFscores
  }, error = function(e) NULL)
}
