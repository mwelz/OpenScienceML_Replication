# -------------------------------------
# Authors: Andreas Alfons and Max Welz
#          Erasmus University Rotterdam
# -------------------------------------


## load relevant functions
source("R/cronbachs_alpha.R")


# function to generate diagonal blocks of a correlation matrix
diagonal_block <- function(scale_size, rho_W) {
  block <- matrix(rho_W, nrow = scale_size, ncol = scale_size)
  diag(block) <- 1
  block
}

#' Generate a correlation matrix in which within-scale correlations are given
#' by a fixed value
#'
#' @param scale_size Number of items in a scale (equal across scales)
#' @param num_scales Number of scales
#' @param rho_W Within-scale correlation (equal across items in a scale)
#'
#' @return A correlation matrix
corrmat_fixed <- function(scale_size, num_scales, rho_W) {

  # initializations
  num_items <- num_scales * scale_size
  item_names <- sapply(paste0("X", seq_len(num_scales)), paste,
                       seq_len(scale_size), sep = "_")

  # generate block diagonal correlation matrix
  out <- matrix(0, nrow = num_items, ncol = num_items)
  seq_scale_size <- seq_len(scale_size)
  for (i in seq_len(num_scales)) {
    indices <- seq_scale_size + (i-1) * (scale_size)
    out[indices, indices] <- diagonal_block(scale_size = scale_size,
                                            rho_W = rho_W)
  }

  # add names and return correlation matrix
  dimnames(out) <- list(item_names, item_names)
  out

}


#' Generate data from a design in which within-scale correlations are given
#' by a fixed value
#'
#' @param n sample size
#'
#' @return list containing the data and the keys of the items
generate_data_fixed <- function(n, scale_size, num_scales,
                                probabilities_center,
                                probabilities_skewed,
                                probabilities_polar,
                                rho_W) {

  # initializations
  num_items <- num_scales * scale_size
  num_likert <- length(probabilities_center)

  # generate correlation matrix
  Rho <- corrmat_fixed(scale_size = scale_size, num_scales = num_scales,
                       rho_W = rho_W)

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
  data <- simstudy::genOrdCat(temp,
                              baseprobs = item_probabilities,
                              prefix = "item",
                              corMatrix = Rho)

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
