# -------------------------------------
# Authors: Max Welz and Andreas Alfons
#          Erasmus University Rotterdam
# -------------------------------------


## start time
tm <- Sys.time()

## load relevant functions
source("R/carelessness_scores.R")
source("R/dgp_grouped.R")
source("R/contamination.R")
source("R/tf.R")  # may cause some irrelevant compiler noise

## directory in which to store results and prefix for RData file
dir_save  <- "simulations/results/"
prefix <- "PoC"


## simulation parameters
num_cores  <- 10    # number of CPU cores for parallel computing
R          <- 1000L # number of replications
n          <- 400L  # number of participants
scale_size <- 8L    # scale size (number of items in a scale)
group_size <- 6L    # group size (number of scales in a group)
num_groups <- 5L    # number of scale groups

## parameters for data generation
design <- "grouped"
# response distributions
probabilities_center <- c(0.15, 0.2, 0.3, 0.2, 0.15)     # centered
probabilities_skewed <- c(0.1, 0.15, 0.2, 0.25, 0.3)     # skewed
probabilities_polar  <- c(0.3, 0.175, 0.05, 0.175, 0.3)  # polarizing
# intervals from which to draw correlations
interval_W  <- c(0.4, 0.6)  # within-scale correlation interval
interval_B  <- c(0.0, 0.2)  # between-scale correlation interval

## compute other relevant parameters
num_scales <- num_groups * group_size       # number of scales
num_items  <- num_scales * scale_size       # number of items
num_likert <- length(probabilities_center)  # number of response categories

## parameters for careless responding
careless_type <- "random"
# prevalence of careless respondents
prop_careless <- c(0.05, 0.10, 0.15, 0.2, 0.25, 0.3)
# onset of careless responding
careless_onset <- "throughout"
interval_onset <- c(1, floor(0.8 * num_items))


# specify hidden layers and batch size for the autoencoder
hidden_layers <- c(1.5 * num_items, num_scales, 1.5 * num_items)
batch_size    <- 10L


## start simulation and repeat R times
set.seed(62183214, "L'Ecuyer")
results_list <- parallel::mclapply(
  mc.cores = num_cores,
  X = seq_len(R),
  FUN = function(r) {

    # generate uncontaminated data
    data_uncontam_list <- generate_data_grouped(
      n = n,
      scale_size = scale_size,
      group_size = group_size,
      num_groups = num_groups,
      probabilities_center = probabilities_center,
      probabilities_skewed = probabilities_skewed,
      probabilities_polar = probabilities_polar,
      interval_W = interval_W,
      interval_B = interval_B
    )
    # extract data matrix
    data_uncontam <- data_uncontam_list$data

    # sample and store indices of careless respondents
    careless_idx <- sample_careless_idx(n = n, prop_careless = prop_careless)

    # randomly sample item order
    item_order <- sample.int(num_items, size = num_items, replace = FALSE)

    # uncontaminated dataset with random item order
    data_uncontam_shuffled <- data_uncontam[, item_order]

    # loop over carelessness prevalence to get results for current repetition
    results <- lapply(seq_along(prop_careless), function(i) {

      # contaminate responses
      data_contam_shuffled <-
        contaminate_data(data = data_uncontam_shuffled,
                         idx = careless_idx[[i]],
                         num_likert = num_likert,
                         interval_onset = interval_onset,
                         type = careless_type)

      # shuffle back in sequential order (required for personal reliability)
      data_contam_sequential <- data_contam_shuffled[, order(item_order)]

      ## autoencoder
      # fit autoencoder
      autoencoder_list <- autoencoder(data = data_contam_shuffled,
                                      hidden_layers = hidden_layers,
                                      activation = c("tanh", "linear", "tanh"),
                                      batch_size = batch_size,
                                      epochs = 100,
                                      loss = pseudo_huber_loss,
                                      optimizer = optimizer_sgd(learning_rate = 1e-04),
                                      verbose = 0)
      # calculate carelessness scores
      autoencoder_scores <- MSRE(x = data_contam_shuffled,
                                 reconstructed = autoencoder_list$reconstructed,
                                 num_likert = num_likert)

      ## squared Mahalanobis distance
      mahalanobis_scores <- mahalanobis_sq(x = data_contam_shuffled)

      ## personal reliability (items must be sequentially ordered)
      scale_sizes <- rep.int(scale_size, times = num_scales)
      reliability_scores <- reliability(x = data_contam_sequential,
                                        scale_sizes = scale_sizes)

      ## synonyms
      synonym_scores <- synonym(x = data_contam_shuffled, min_corr = 0.6)

      ## IRV
      irv_scores <- irv(x = data_contam_shuffled)

      ## lz person-fit
      lz_scores <- lz(x = data_contam_shuffled, num_likert = num_likert)

      # return containing the scores of each method: we use a list because in
      # case of an error, the above functions return NULL
      list(autoencoder = autoencoder_scores, mahalanobis = mahalanobis_scores,
           reliability = reliability_scores, synonym = synonym_scores,
           irv = irv_scores, lz = lz_scores)

    })

    # add carelessness prevalence as names for list of results
    names(results) <- prop_careless

    # print progress
    cat("\n", format(Sys.time(), usetz = TRUE), ": Done with repetition ", r,
        "\n\n", sep = "")

    # return results together with other relevant information from current
    # repetition as list
    list(results = results,
         careless_idx = careless_idx,
         cronbachs_alpha = data_uncontam_list$cronbachs_alpha)

  }
)


## save output
file_save <- paste0(dir_save, prefix, "_", design, "_", careless_type, "_",
                    careless_onset, "_R=", R, ".Rdata")
save(results_list, n, scale_size, num_scales, num_items, careless_type,
     prop_careless, file = file_save)

## print running time
run_time <- Sys.time() - tm
print(run_time)
