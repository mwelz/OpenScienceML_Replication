# -------------------------------------
# Authors: Andreas Alfons and Max Welz
#          Erasmus University Rotterdam
# -------------------------------------


## load relevant functions
source("R/cronbachs_alpha.R")


# function to generate diagonal blocks of correlation matrix
diagonal_block <- function(scale_size, interval_W) {
  block <- diag(1, scale_size)
  lower <- lower.tri(block)
  upper <- upper.tri(block)
  block[lower] <- runif(scale_size * (scale_size-1) / 2,
                        min = interval_W[1], max = interval_W[2])
  block[upper] <- t(block)[upper]
  block
}


#' Generate a correlation matrix in which within-scale correlations are drawn
#' randomly from a given interval
#'
#' @param scale_size Number of items in a scale (equal across scales)
#' @param num_scales Number of scales
#' @param interval_W Interval from which unique within-scale correlations are
#' randomly drawn
#' @return A positive-definite correlation matrix
corrmat_interval <- function(scale_size, num_scales, interval_W) {

  ## initializations
  num_items <- num_scales * scale_size
  item_names <- sapply(paste0("X", seq_len(num_scales)), paste,
                       seq_len(scale_size), sep = "_")

  ## draw correlation matrix until it can be corrected to be positive-definite
  continue_while <- TRUE
  seq_scale_size <- seq_len(scale_size)
  seq_num_scales <- seq_len(num_scales)
  while (continue_while) {

    # generate block diagonal correlation matrix
    out <- matrix(0, nrow = num_items, ncol = num_items)
    for (i in seq_num_scales) {
      indices <- seq_scale_size + (i-1) * (scale_size)
      out[indices, indices] <- diagonal_block(scale_size = scale_size,
                                              interval_W = interval_W)
    }

    # compute the nearest positive-definite correlation matrix
    nearPD_object <- Matrix::nearPD(out, corr = TRUE,
                                    keepDiag = TRUE,
                                    ensureSymmetry = TRUE,
                                    base.matrix = TRUE)

    # break while loop if we found a positive-definite matrix
    # (that is, algorithm converged)
    continue_while <- !nearPD_object$converged

  }

  ## extract final correlation matrix and add names
  out <- nearPD_object$mat
  dimnames(out) <- list(item_names, item_names)
  out

}


#' Generate data from a design in which within-scale correlations are drawn
#' randomly from a given interval
#'
#' @param n sample size
#'
#' @return list containing the data and the keys of the items
generate_data_interval <- function(n, scale_size, num_scales,
                                   probabilities_center,
                                   probabilities_skewed,
                                   probabilities_polar,
                                   interval_W) {

  # initializations
  num_items <- num_scales * scale_size
  num_likert <- length(probabilities_center)

  # generate correlation matrix
  Rho <- corrmat_interval(scale_size = scale_size, num_scales = num_scales,
                          interval_W = interval_W)

  # distributions within scale
  scale_center <- do.call(rbind, rep(list(probabilities_center), scale_size))
  scale_skewed <- do.call(rbind, rep(list(probabilities_skewed), scale_size))
  scale_polar  <- do.call(rbind, rep(list(probabilities_polar), scale_size))

  # distributions for all items
  # NOTE: number of items per scale ('scale_size') must be a multiple of 3
  #       (as we have three types of distributions)
  all_types <- list(scale_center, scale_skewed, scale_polar)
  item_probabilities <- do.call(rbind, rep(all_types, each = num_scales/3))

  # compute Cronbach's alpha of the scales
  alpha <- cronbachs_alpha(Rho = Rho, item_probabilities = item_probabilities,
                           scale_size = scale_size, num_scales = num_scales)

  # generate data
  temp <- simstudy::genData(n)
  data <- simstudy::genOrdCat(temp, baseprobs = item_probabilities,
                              prefix = "item", corMatrix = Rho)

  # drop id column and convert to matrix
  data <- as.matrix(data[, -1])
  data <- matrix(as.integer(data), nrow = n)

  # make positively and negatively keyed items within each scale
  idx_neg_keyed <- as.numeric(
    sapply(0:(num_scales-1), function(i) {
      1:floor(scale_size/2) + i * scale_size
    })
  )

  # adjust for negatively keyed items
  data[, idx_neg_keyed] <- num_likert - data[, idx_neg_keyed] + 1
  colnames(data) <- colnames(Rho)
  keys <- rep(1L, num_items)
  keys[idx_neg_keyed] <- -1L

  # return data and keys as list
  list(data = data, cronbachs_alpha = alpha, keys = keys)

}
