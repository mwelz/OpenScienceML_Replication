# -------------------------------------
# Authors: Andreas Alfons and Max Welz
#          Erasmus University Rotterdam
# -------------------------------------


## load relevant functions
source("R/cronbachs_alpha.R")


# function to generate diagonal blocks of a correlation matrix
diagonal_block <- function(scale_size, interval_W) {
  block <- diag(1, scale_size)
  lower <- lower.tri(block)
  upper <- upper.tri(block)
  block[lower] <- runif(scale_size * (scale_size-1) / 2,
                        min = interval_W[1], max = interval_W[2])
  block[upper] <- t(block)[upper]
  block
}

# function to generate offdiagonal blocks of a correlation matrix
offdiagonal_block <- function(scale_size, interval_B) {
  matrix(runif(scale_size^2, min = interval_B[1], max = interval_B[2]),
         nrow = scale_size, ncol = scale_size)
}


#' Generate a block correlation matrix
#'
#' @param scale_size Number of items in a scale (equal across scales)
#' @param num_scales Number of scales
#' @param interval_W Interval from which unique within-scale correlations are
#' randomly drawn
#' @param interval_B Interval from which unique between-scale correlations are
#' randomly drawn
#'
#' @return A positive-definite correlation matrix
corrmat_block <- function(scale_size, num_scales,
                          interval_W, interval_B) {

  ## draw correlation matrix until it can be corrected to be positive-definite
  continue_while <- TRUE
  while (continue_while) {

    # initialize correlation matrix as a list of lists
    # (outer list corresponds to columns, inner list to rows of the given column)
    out <- replicate(num_scales,
                     replicate(num_scales, NULL, simplify = FALSE),
                     simplify = FALSE)
    # loop over indices of blocks in the correlation matrix, and generate those
    # blocks following the R convention of building a matrix by column
    # (k is the index of the row, l is the index of the column)
    seq_scales <- seq_len(num_scales)
    for (l in seq_scales) {
      for (k in seq_scales) {
        if (k == l) {
          out[[l]][[k]] <- diagonal_block(scale_size, interval_W)
        } else if (k > l) {
          out[[l]][[k]] <- offdiagonal_block(scale_size, interval_B)
        } else out[[l]][[k]] <- t(out[[k]][[l]])
      }
    }
    # put correlation matrix together
    out <- do.call(cbind, lapply(out, function(column) do.call(rbind, column)))

    # compute the nearest positive-definite correlation matrix
    nearPD_object <- Matrix::nearPD(out, corr = TRUE,
                                    keepDiag = TRUE,
                                    ensureSymmetry = TRUE,
                                    base.matrix = TRUE)

    # break while loop if we found a positive-definite matrix
    # (that is, algorithm converged)
    continue_while <- !nearPD_object$converged

  }

  ## extract final correlation matrix
  nearPD_object$mat

}


#' Generate a correlation matrix for a grouped scale design
#'
#' @param scale_size Number of items in a scale (equal across scales)
#' @param group_size Number of scales in a group (equal across groups)
#' @param num_groups Number of (uncorrelated) groups of scales
#' @param interval_W Interval from which unique within-scale correlations are
#' randomly drawn
#' @param interval_B Interval from which unique between-scale correlations are
#' randomly drawn
#'
#' @return A correlation matrix
corrmat_grouped <- function(scale_size, group_size, num_groups,
                            interval_W, interval_B) {

  # initializations
  num_scales <- num_groups * group_size
  num_items <- num_scales * scale_size
  out <- matrix(0, nrow = num_items, ncol = num_items)
  item_names <- sapply(paste0("X", seq_len(num_scales)), paste,
                       seq_len(scale_size), sep = "_")

  # generate block diagonal correlation matrix for each group
  num_items_per_group <- group_size * scale_size
  seq_items_per_group <- seq_len(num_items_per_group)
  for (i in seq_len(num_groups)) {
    indices <- seq_items_per_group + (i-1) * (num_items_per_group)
    out[indices, indices] <- corrmat_block(scale_size = scale_size,
                                           num_scales = group_size,
                                           interval_W = interval_W,
                                           interval_B = interval_B)
  }

  # add names and return correlation matrix
  dimnames(out) <- list(item_names, item_names)
  out

}


#' Generate data from a grouped scale design
#'
#' @param n sample size
#'
#' @return list containing the data and the keys of the items
generate_data_grouped <- function(n, scale_size, group_size, num_groups,
                                  probabilities_center, probabilities_skewed,
                                  probabilities_polar, interval_W, interval_B) {

  # initializations
  num_scales <- num_groups * group_size
  num_items <- num_scales * scale_size
  num_likert <- length(probabilities_center)

  # generate correlation matrix
  Rho <- corrmat_grouped(scale_size = scale_size,
                         group_size = group_size,
                         num_groups = num_groups,
                         interval_W = interval_W,
                         interval_B = interval_B)

  # distributions within scale
  scale_center <- do.call(rbind, rep(list(probabilities_center), scale_size))
  scale_skewed <- do.call(rbind, rep(list(probabilities_skewed), scale_size))
  scale_polar  <- do.call(rbind, rep(list(probabilities_polar), scale_size))

  # distributions within group
  # NOTE: number of scales per group ('group_size') must be a multiple of 3
  #       (as we have three types of distributions)
  all_types <- list(scale_center, scale_skewed, scale_polar)
  group_probabilities <- do.call(rbind, rep(all_types, each = group_size/3))

  # distributions for all items
  item_probabilities <- do.call(rbind, rep(list(group_probabilities), num_groups))

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
