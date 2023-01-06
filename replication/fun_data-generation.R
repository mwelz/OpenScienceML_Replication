# --------------------------------------------------------------------------------------------------------------------
# here are all functions needed to generate data according to our simulation design
#
# Last changed on December 12, 2022
# --------------------------------------------------------------------------------------------------------------------


#' returns block matrix of within-construct correlations.
#' 
#' The off-diagonal elements of each block equal a unique construct-code and can be replaced by within-construct correlations.
#' 
#' @param construct_size Number of items in a construct (equal across constructs)
#' @param num_constructs Number of constructs
#' 
#' 
get_blockmatrix <- function(construct_size, num_constructs){
  
  num_items <- construct_size * num_constructs
  out <- matrix(0.0, nrow = num_items, ncol = num_items)
  nam <- rep(NA_character_, num_items)
  
  for(i in 0:(num_constructs-1)){
    
    interval <- 1:construct_size + i * construct_size
    out[interval, interval] <- i + 2
    nam[interval] <- paste0(paste0("X", i + 1), "_", 1:construct_size)
    
    
  } # FOR
  
  diag(out) <- 1.0 # diagonal is one
  colnames(out) <- rownames(out) <- nam
  out
  
} # FUN


#' Sample indices of careless respondents.
#' 
#' Returns list of indices for each level of carelessness, as governed by \code{prop_careless_arr}. Note: the indices are nested, meaning that indices for a lower level of carelessness will also be indices in all higher levels of carelessness
#' 
#' @param n sample size
#' @param prop_careless_arr A numeric vector with elements in [0,1] denoting the proportions of careless respondents in the sample of size \code{n} that we are interested in.
careless_location <- function(n, prop_careless_arr)
{
  # initialize
  out      <- list()
  idx_arr <- seq_len(n)
  
  # randomly sample indices
  idx_contam <- sample(x = idx_arr, 
                       size = floor(max(prop_careless_arr) * n), 
                       replace = FALSE)
  num_careless <- floor(prop_careless_arr * n)
  
  for(i in seq_along(prop_careless_arr))
  {
    if(prop_careless_arr[i] == 0L)
    {
      out[[i]] <- integer() # empty if no one is careless
    } else
    {
      out[[i]] <- idx_contam[seq_len(num_careless[i])]
    } # IF
  } # FOR
  
  names(out) <- prop_careless_arr
  out
} # FUN


#' Generate data as described in the paper
#'
#' There are 20 constructs of 10 items each. Items within each construct have correlation -0.7 or 0.7, and items
#' of different constructs are independent
#' 
#' @param n sample size
data_uncontam_dist <- function(n){
  
  construct_size <- 10
  num_constructs <- 20
  num_items <- construct_size * num_constructs
  
  # prepare correlation matrix
  Rho <- get_blockmatrix(construct_size, num_constructs)
  
  # assign correlation of items within same construct
  Rho[Rho %in% 2:(num_constructs + 1L) ] <- 0.7
  
  # check for psd
  if(any(eigen(Rho)$values < 0)) stop("Rho is not postive semi-definite!")
  
  # set answer distribution per item
  num_likert <- 5L                                       # 5 answer categories
  probabilities_center <- c(0.15, 0.2, 0.3, 0.2, 0.15)   # centered about central 
  probabilities_right  <- c(0.1, 0.15, 0.2, 0.25, 0.3)   # left-skewed
  probabilities_left   <- rev(probabilities_right)       # right-skewed
  probabilities_polar  <- c(0.3,0.175, 0.05, 0.175, 0.3) # polarizing
  
  
  # distributions within construct
  construct_center <- Reduce(rbind, rep(list(probabilities_center), construct_size))
  construct_left   <- Reduce(rbind, rep(list(probabilities_left), construct_size))
  construct_right  <- Reduce(rbind, rep(list(probabilities_right), construct_size))
  construct_polar  <- Reduce(rbind, rep(list(probabilities_polar), construct_size))
  
  # there are 5 constructs of each type
  alltypes <- rbind(construct_center, construct_left, construct_right, construct_polar)
  baseprobs <- Reduce(rbind, rep(list(alltypes), 5L))
  
  # make positively and negatively keyed items within each construct
  idx_neg_keyed <- as.numeric(
    sapply(0:(num_constructs-1), function(i){
      1:floor(construct_size/2) + i * construct_size
    }))
  
  # generate data
  temp <- simstudy::genData(n)
  data <- simstudy::genOrdCat(temp,
                              baseprobs = baseprobs,
                              prefix = "q",
                              corMatrix = Rho)
  
  data <- as.matrix(data[,-1]) # drop id
  data <- matrix(as.integer(data), nrow = n)
  
  # adjust for negatively keyed items
  data[,idx_neg_keyed] <- num_likert - data[,idx_neg_keyed] + 1
  colnames(data) <- colnames(Rho)
  keys <- rep(1L, num_items)
  keys[idx_neg_keyed] <- -1L
  
  return(list(data = data, keys = keys))
  
} # FUN


#' Contaminate simulated data as described in the paper
#' 
#' Carelessness onset item is randomly drawn to be within 60% and 80% of all items
#' Careless responses are completely random (same probability for all answer categories)
#' 
#' @param data_contam A matrix of uncontaminated responses (rows are participants, columns items)
#' @param careless_idx A vector of indices of rows in \code{data_contam} that shall be careless respondents
#' @param num_likert Number of Likert-type answer categories 
data_contamination_dist <- function(data_uncontam, 
                                   careless_idx,
                                   num_likert){
  
  data_contam <- data_uncontam
  num_items   <- ncol(data_contam)
  
  # range for onset of carelessness is all items between 60% and 80% of all items
  onset_arr <- seq(from = floor(0.6 * num_items), to = floor(0.8 * num_items), by = 1)
  
  if(num_likert != 5) stop("current implementation only supports 5 likert points")
  
  for(i in seq_along(careless_idx)){
    
    # sample starting point of carelessness by randomly drawing from onset_arr
    onset        <- sample(onset_arr, 1L)
    
    # items that will be carelessly responded to
    careless_arr <- onset:num_items
    
    # sample careless responses as completely random responses
    # (same probability for all answer categories)
    idx <- careless_idx[i]
    data_contam[idx, careless_arr] <- sample.int(num_likert, 
                                                 size = length(careless_arr),
                                                 replace = TRUE)
    
  } # FOR
  
  data_contam
  
} # FUN
