# --------------------------------------------------------------------------------------------------------------------
# functions to fit an autoencoder
#
# Last changed on December 12, 2022, by Max Welz (welz {at} ese.eur.nl)
# --------------------------------------------------------------------------------------------------------------------

# Note: this script requires an installation of the Python libraries TensorFlow and Keras, as well as an installation 
# of Python itself and required dependencies. All components can easily be installed by uncommenting and running the lines below:

## download R interface for keras
# install.packages("keras") # install keras
#
## install Python and required dependencies (including keras and tensorflow)
## Note: you may be asked to install Miniconda if there is no Python installation on your machine. 
## Agree to this if you wish to install Miniconda 
## (An installation of Python or Miniconda are necessary for running this script)
# keras::install_keras(method = "auto", conda = "auto", version = "2.9")  

# load keras
library("keras")


# specify pseudo-Huber loss between true and predicted values with delta = 1
pseudo_huber_loss <- function(y_true, y_predict)
{
  delta <- k_constant(1)
  resid <- y_true - y_predict
  k_sum(k_pow(delta, 2) * (k_sqrt(1 + k_pow(resid / delta, 2)) - 1))
} # FUN


#' Fit an autoencoder without regularization
#' 
#' Returns a list with reconstruction of input data, reconstruction error, and keras object
#' 
#' @param data a numeric matrix of responses. Rows are participants, columns are items
#' @param hidden_layers Numeric vector. Length corresponds to the number of hidden layers. The individual components correspond to the number of nodes/neurons in each layer
#' @param activation Activation functions for each later. Check the documentation of the keras package for options. Default are tanh, identity (called 'linear' in keras), and tanh.
#' @param loss Function object for the loss. Default is pseudo-Huber loss (defined above)
#' @param optimizer Optimization algorithm to be used. Default is adam.
#' @param epochs Number of epochs
#' @param batch_size Batch size for batch learning
#' @param verbose Shall learning loss be plotted? Default is 0 for no.
#' @param seed Seed for pseudo random number generation
autoencoder <- function(data,
                        hidden_layers = c(10, 2, 10),
                        activation = c("tanh", "linear", "tanh"),
                        loss = pseudo_huber_loss,
                        optimizer = keras::optimizer_adam(learning_rate = 1e-04),
                        epochs = 100L,
                        batch_size = 32L,
                        verbose = 0L,
                        seed = NULL)
{
  stopifnot(!any(is.na(data)) && is.numeric(data))
  
  ## standardize data (this is standard in deep learning)
  data        <- as.matrix(data)
  input_size  <- ncol(data)
  m           <- colMeans(data)
  s           <- sapply(1:input_size, function(j) sqrt(var(data[,j])) )
  X           <- sapply(1:input_size, function(j) (data[,j] - m[j]) / s[j] )
  colnames(X) <- colnames(data)
  
  ## prepare the layers
  num_hl <- length(hidden_layers) # number of hidden layers
  stopifnot(length(activation) == num_hl)
  layers <- lapply(1:(num_hl + 1L), function(j){
    
    if(j == 1){
      
      # input layer
      layer_dense(units = hidden_layers[j], 
                  activation = activation[j],
                  input_shape = input_size, 
                  kernel_regularizer = NULL,
                  bias_regularizer = NULL)
      
    } else if(j == num_hl + 1L){
      
      # output layer
      layer_dense(units = input_size,
                  activation = "linear")
      
    } else {
      
      # hidden layers
      layer_dense(units = hidden_layers[j], 
                  activation = activation[j])
    } # IF
    
  }) # SAPPLY
  
  
  ## define the autoencoder
  model <- keras_model_sequential(layers)
  
  
  ## details of the optimizer
  model %>% compile(
    loss      = loss,
    optimizer = optimizer
  )
  
  
  ## specify random seed
  if(!is.null(seed)){
    tensorflow::set_random_seed(seed)
  }
  
  
  ## fit the model
  model %>% fit(X, X,
                epochs = epochs,
                batch_size = batch_size, 
                verbose = verbose)
  
  ## make in-sample prediction
  Xhat <- model %>% predict(X)
  
  ## de-standardize the predictions
  Xhat_ds <- sapply(1:input_size, function(j) Xhat[,j] * s[j] + m[j] )
  colnames(Xhat_ds) <- colnames(data)
  
  ## reconstruction error
  rec_err <-  sapply(1:nrow(X), function(i) mean((Xhat_ds[i,] - data[i,])^2) )
  
  return(list(reconstructed = Xhat_ds,
              reconstruction_error = rec_err,
              model = model))
  
} # FUN
