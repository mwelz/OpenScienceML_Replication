# ------------------------------------------------------------------------------
# Analyze the simulation results generated: compute recall curves

# Created on Jul 12, 2023, by Max Welz (welz {at} ese.eur.nl)
# Last changed on August 8, 2023, by Max Welz
# ------------------------------------------------------------------------------


## load relevant functions
source("R/recall.R")

## directories to load and save results in
dir_load <- "simulations/results/"
prefix <- "PoC"
dir_save <- "simulations/analyzed/"

## simulation settings to loop over
designs <- c("idealized", "high", "moderate", "low", "grouped")
careless_types <- c("random", "extreme", "pattern", "straight")
careless_onsets <- c("throughout", "late")

## number of replications
R <- 1000L


## loop over different settings
for (design in designs) {
  for (careless_type in careless_types) {
    for (careless_onset in careless_onsets) {

      ## load data
      file_load <- paste0(dir_load, prefix, "_", design, "_", careless_type,
                          "_", careless_onset, "_R=", R, ".Rdata")
      if (file.exists(file_load)) {
        load(file_load)

        ## initializations

        # extract names of methods
        methods <- names(results_list[[1]]$results[[1]])

        # specify whether low or high values of the scores are indicative
        # of careless responding
        if (careless_type %in% c("pattern", "straight")) {
          # straightlining or pattern responding
          indicative <- c(autoencoder = "low", mahalanobis = "high",
                          reliability = "low", synonym = "low",
                          irv = "low", lz = "low")
        } else {
          # random or extreme responding
          indicative <- c(autoencoder = "high", mahalanobis = "high",
                          reliability = "low", synonym = "low",
                          irv = "high", lz = "low")
        }

        ## order carelessness scores and compute recall curves

        # create list of data frames with mean recall for each method
        mean_recall_list <- lapply(methods, function(method) {

          # create list of data frame with mean recall for current method
          tmp <- lapply(seq_along(prop_careless), function(i) {

            # compute recall for all repetitions
            recall_list <- lapply(seq_len(R), function(r) {

              # extract scores and indices of true careless respondents
              scores <- results_list[[r]]$results[[i]][[method]]
              careless_idx <- results_list[[r]]$careless_idx[[i]]

              # compute recall curve
              # Note: The carelessness scores are NULL in case of an error
              if (!is.null(scores)) {
                recall_curve <- recall(x = scores, idx = careless_idx,
                       indicative = indicative[method])
              }

            })

            # combine recall curves from different repetitions into matrix
            recall_mat <- do.call(rbind, recall_list)
            # Note: The matrix is NULL in case of an error in every repetition
            if (!is.null(recall_mat)) {
              # average the recall curves by taking the column means
              mean_recall <- colMeans(recall_mat)
              # return data frame with current recall curve
              data.frame(Method = method,
                         Prevalence = prop_careless[i],
                         Identified = seq_along(mean_recall),
                         Recall = mean_recall)
            }

          })

          # combine results into one data frame for current method
          do.call(rbind, tmp)

        })

        # combine results into one data frame- for all methods
        df_mean_recall <- do.call(rbind, mean_recall_list)

        # save results for recall curves
        file_recall <- paste0(dir_save, "recall_", design, "_", careless_type,
                              "_", careless_onset, "_R=", R, ".Rdata")
        save(df_mean_recall, methods, n, num_items, prop_careless,
             file = file_recall)


        ## extract Cronbach's alpha (rows are replications, columns are scales)

        # obtain range of Cronbach's alpha
        alpha <- do.call(rbind, lapply(results_list, "[[", "cronbachs_alpha"))
        range_alpha <- range(alpha)

        # save Cronbach's alpha values for reporting
        file_alpha <- paste0(dir_save, "alpha_", design, "_", careless_type,
                             "_", careless_onset, "_R=", R, ".Rdata")
        save(range_alpha, alpha, file = file_alpha)

      }

    }
  }
}
