# --------------------------------------------------------------------------------------------------------------------
# Here we analyze the results of the simulation. Concretely, we compute the recall curves for each run and average across
# the repetitions
#
# Last changed on December 12, 2022
# --------------------------------------------------------------------------------------------------------------------
# rm(list = ls()) ; cat("\014") ; gc()

# load relevant functions and simulation output
source("fun_carelessness.R")
load("simulation.Rdata")

# number of repetitions ans items
R         <- length(careless_location_ls)
num_items <- num_constructs * construct_size


# prepare objects to store results in
methods <- c("reliability", "synonym", "antonym", "mah2_ord", "RE") # considered methods
arr_out    <- array(NA_real_, dim = c(length(prop_careless_arr), n, R))
ls_recall  <- rep(list(arr_out), length(methods))
names(ls_recall) <- methods



## bring results in usable format
for(i in seq_along(prop_careless_arr))
{
  for(r in seq_len(R))
  {
    
    ## calculate recall for each method
    ls_recall$reliability[i,,r] <- recall(x = reliability_arr[i,,r],
                                          contam = careless_location_ls[[r]][[i]], 
                                          right = FALSE)
    ls_recall$synonym[i,,r]     <- recall(x = synonym_arr[i,,r],
                                          contam = careless_location_ls[[r]][[i]], 
                                          right = FALSE)
    ls_recall$antonym[i,,r]     <- recall(x = antonym_arr[i,,r],
                                          contam = careless_location_ls[[r]][[i]], 
                                          right = TRUE)
    ls_recall$mah2_ord[i,,r]    <- recall(x = mah2_ord_arr[i,,r],
                                          contam = careless_location_ls[[r]][[i]], 
                                          right = TRUE)
    ls_recall$RE[i,,r]          <- recall(x = RE_arr[i,,r],
                                          contam = careless_location_ls[[r]][[i]], 
                                          right = TRUE)
    
  } # FOR r
} # FOR i


## take averages over repetitions
recall_ave <- 
  lapply(seq_along(methods), function(j){
    tmp <- apply(ls_recall[[j]], c(1,2), mean) 
    rownames(tmp) <- prop_careless_arr
    colnames(tmp) <- seq_len(n)
    tmp
  })
names(recall_ave) <- methods


# save results
save(recall_ave, prop_careless_arr, n,
     file = "simulation_analyzer.Rdata")
