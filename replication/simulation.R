# --------------------------------------------------------------------------------------------------------------------
# Here we run the simulation described in the main paper. We sample n = 400 participants who respond to p = 200 items. 
# There are 20 constructs of 10 items each. Items within each construct have correlation -0.7 or 0.7, and items
# of different constructs are independent. We generate 1000 data sets, which we then contaminate with various levels
# of careless respondents. We save the carelessness scores of each run.
#
# Last changed on December 12, 2022
# --------------------------------------------------------------------------------------------------------------------
# rm(list = ls()) ; cat("\014") ; gc()
tm <- Sys.time()

# load relevant functions
source("fun_carelessness.R")
source("fun_data-generation.R")
source("fun_autoencoder.R") # may cause some irrelevant compiler noise

# simulation parameters
R                 <- 1000L # number of repetitions
n                 <- 400L  # number of participants
construct_size    <- 10L   # construct size (= number of items in a scale)
num_constructs    <- 20L   # number of constructs
num_likert        <- 5L    # number of Likert-type response categories
prop_careless_arr <- c(0.05, 0.10, 0.15, 0.2, 0.25, 0.3) # proportions of careless responding to consider


# number of items and directory to store results in
num_items <- construct_size * num_constructs
dir_save  <- "simulation.Rdata"


# specify hidden layers and batch size for the autoencoder
hidden_layers <- c(1.5 * num_items, num_constructs, 1.5 * num_items)
batch_size    <- 10L

# sample seeds for data generation
set.seed(62183214)
seeds <- as.integer(round(1e8 * runif(R), 0))

# initialize arrays and lsists to store results in
RE_arr      <- array(NA_real_, dim = c(length(prop_careless_arr), n, R))
synonym_arr <- antonym_arr <- reliability_arr <- mah2_ord_arr <- 
  RE_arr
careless_location_ls <- list() # keep the indices of careless respondents


## start simulation and repeat R times
for(r in seq_len(R))
{
  # generate uncontaminated data
  set.seed(seeds[r])
  data_uncontam_ls <- data_uncontam_dist(n)
  data_uncontam.   <- data_uncontam_ls$data # get data matrix 
  keys             <- data_uncontam_ls$keys # get keys of each item (1 if positively worded, -1 if negatively worded)
  
  # sample and store indices of careless respondents
  careless_location. <- 
    careless_location(n = n, 
                      prop_careless_arr = prop_careless_arr)
  careless_location_ls[[r]] <- careless_location.
  
  # randomly sample item order
  item_order <- sample(1:num_items, size = num_items, replace = FALSE)
  
  # uncontaminated dataset with random item order
  data_uncontam_shuffle <- data_uncontam.[,item_order]
  

  # loop over carelessness proportions
  for(i in seq_along(prop_careless_arr))
  {
    # contaminate responses 
    careless_idx <- careless_location.[[i]]
    data_contam_shuffle  <- data_contamination_dist(data_uncontam = data_uncontam_shuffle, 
                                                   careless_idx = careless_idx, 
                                                   num_likert = num_likert)
    
    # shuffle back in previous (sequential) order; required for personal reliability
    data_contam_sequential <- data_contam_shuffle[,order(item_order)]
    

    ## autoencoder
    autoencoder_ls <- autoencoder(data = data_contam_shuffle, 
                                  hidden_layers = hidden_layers, 
                                  activation = c("tanh", "linear", "tanh"), 
                                  batch_size = batch_size,
                                  epochs = 100, 
                                  loss = pseudo_huber_loss,
                                  optimizer = optimizer_sgd(learning_rate = 1e-04),
                                  verbose = 0, 
                                  seed = 12345)
    
    # reconstruct data and calculate carelessness scores
    reconstructed   <- autoencoder_ls$reconstructed
    RE.             <- ((data_contam_shuffle - reconstructed) / num_likert)^2
    RE_arr[i,,r]    <- RE(RE.) 
    
    ## synonyms & antonyms:
    synonym_arr[i,,r] <- synonym(x = data_contam_shuffle, mincorr = 0.6)
    antonym_arr[i,,r] <- antonym(x = data_contam_shuffle, mincorr = -0.6)
    
    ## personal reliability (items must be sequentially ordered)
    reliability_arr[i,,r] <- reliability(x = data_contam_sequential, 
                                constructs = rep(construct_size, num_constructs))
    
    ## ordinary squared Mahalanobis distance
    mah2_ord_arr[i,,r] <- mahalanobis_sq(x = data_contam_shuffle)
  
  } # FOR i in prop_careless_arr
  
  print(paste0("Done with iteration ", r, " at ", Sys.time()))
  
} # FOR r

# save output
save(RE_arr, synonym_arr, antonym_arr, reliability_arr, 
     mah2_ord_arr, prop_careless_arr, n, num_items, careless_location_ls,
     construct_size, num_constructs,
     file = dir_save)

Sys.time() - tm # runtime
