# -------------------------------------
# Authors: Max Welz and Andreas Alfons
#          Erasmus University Rotterdam
# -------------------------------------


#' Sample indices of careless respondents
#'
#' @param n Sample size
#' @param prop_careless_arr A numeric vector with elements in [0,1] denoting
#' the proportions of careless respondents in the sample of size \code{n} that
#' we are interested in.
#'
#' @return A list of indices for each level of carelessness, as governed
#' by \code{prop_careless}.
#'
#' @note The indices are nested, meaning that indices for a lower level of
#' carelessness will also be indices in all higher levels of carelessness.
sample_careless_idx <- function(n, prop_careless) {

  # randomly sample indices
  num_careless <- floor(prop_careless * n)
  idx_careless <- sample.int(n = n,
                             size = max(num_careless),
                             replace = FALSE)

  # construct list of sampled indices for different proportions of carelessness
  out <- lapply(num_careless, function(num) {
    if (num == 0L) integer()
    else idx_careless[seq_len(num)]
  })

  # add names and return indices
  names(out) <- prop_careless
  out

}


#' Draw careless responses on the individual level
#' @param num_likert Number of Likert-type answer categories
#' @param num_items Number of contaminated responses to be samples
#' @param type Type of contamination. Either "random" (responses drawn from all
#' categories with equal probability), "extreme" (responses drawn from the two
#' extreme categories with equal probability), "pattern" (select a combination
#' of response categories at random, permute them, and repeat this pattern in
#' the items), or "straight" (one response category is selected at random, then
#' the selected response is used for all items).
generate_careless_responses <- function(num_likert, num_items, type) {

  # initializations
  type <- match.arg(type, choices = c("random", "extreme", "pattern", "straight"))

  # generate careless responses
  if (type == "random") {
    # choose completely at random
    out <- sample.int(num_likert, size = num_items, replace = TRUE)
  } else if (type == "extreme") {
    # choose randomly between extreme response categories
    out <- sample(c(1L, num_likert), size = num_items, replace = TRUE)
  } else if (type == "pattern") {
    # randomly select a combination of response categories of any subset size
    # (except single response categories to exclude staightliners)
    seq_likert <- seq_len(num_likert)
    combination_list <- lapply(seq_likert[-1L], function(m) {
      combn(seq_likert, m, simplify = FALSE)
    })
    combinations <- do.call(c, combination_list)
    keep <- sample.int(length(combinations), size = 1L)
    selected <- combinations[[keep]]
    # permute the selected combination (if more than one response category)
    if (length(selected) > 1L) selected <- sample(selected, replace = FALSE)
    # repeat the selected pattern
    out <- rep_len(selected, length.out = num_items)
  } else if (type == "straight") {
    # always choose same (randomly determined) option
    response <- sample.int(num_likert, size = 1L)
    out <- rep.int(response, times = num_items)
  }

  # return responses
  out
}


#' Contaminate data
#'
#' Carelessness onset item is randomly drawn to be within a certain interval of
#' items.  Careless responses can be generated in different ways.
#'
#' @param data A matrix of responses to be contaminated (rows are participants,
#' columns are items).
#' @param idx A vector of indices of rows in \code{data_contam} that
#' are turned into careless respondents.
#' @param num_likert Number of Likert-type answer categories.
#' @param interval_onset A numeric vector of length two giving an interval
#' of items. The onset of carelessness of each careless respondent is drawn
#' randomly on this interval.
#' @param type Type of contamination. Either "random" (responses drawn from all
#' categories with equal probability), "extreme" (responses drawn from the two
#' extreme categories with equal probability), or "straight" (one response
#' category is selected at random, then the selected response is used for all
#' remaining items).
contaminate_data <- function(data, idx, num_likert,
                             interval_onset, type) {

  # initializations
  data_contam <- data
  num_items   <- ncol(data)

  # range of possible carelessness onset items
  seq_onset <- seq(from = interval_onset[1L], to = interval_onset[2L], by = 1L)

  # loop over careless respondents
  for (i in idx) {

    # sample starting point of carelessness
    onset <- sample(seq_onset, size = 1L)

    # items to which the respondent will respond carelessly
    seq_careless <- seq(from = onset, to = num_items, by = 1L)

    # sample careless responses as completely random responses
    # (same probability for all answer categories)
    data_contam[i, seq_careless] <-
      generate_careless_responses(num_likert = num_likert,
                                  num_items = length(seq_careless),
                                  type = type)

  }

  # return contaminated data
  data_contam

}
