# -------------------------------------
# Authors: Max Welz and Andreas Alfons
#          Erasmus University Rotterdam
# -------------------------------------


#' Calculate recall curve
#' @param x A numeric vector of carelessness scores for each participant
#' @param idx A vector of indices in \code{x} that are associated with
#' careless respondents
#' @param indicative A character string specifying whether high or low scores
#' are indicative of careless responding
#' @param right If \code{TRUE}, then we expect scores of careless respondents
#' to be in right tail (higher values of \code{x} are indicative of
#' carelessness). If FALSE, then lower values are indicative of carelessness.
recall <- function(x, idx, indicative = c("high", "low")) {
  # initializations
  indicative <- match.arg(indicative)
  # order observations
  decreasing <- indicative == "high"
  x_ordered <- order(x = x, decreasing = decreasing)
  # compute recall
  sapply(seq_along(x_ordered), function(i) mean(idx %in% x_ordered[seq_len(i)]))
}
