## calculate Cronbach's alpha on population level

# distribution of answer categories
p <- c(0.15, 0.2, 0.3, 0.2, 0.15)
# p <- c(0.1, 0.15, 0.2, 0.25, 0.3)
# p <- c(0.3, 0.25, 0.2, 0.15, 0.1)
# p <- c(0.3, 0.175, 0.05, 0.175, 0.3)

# number of items and within-construct correlation matrix
k <- 10
R <- matrix(0.7, nrow = k, ncol = k)
diag(R) <- 1

# compute variance of each item
x <- seq_along(p)
EX <- sum(p * x)
EX2 <- sum(p * x^2)
sigma2_item <- EX2 - EX^2

# compute covariance matrix (simplified since all items have the same variance)
Sigma <- sigma2_item * R

# compute variance of total score
ones <- rep.int(1, k)
sigma2_total <- drop(t(ones) %*% Sigma %*% ones)

# compute Cronbach's alpha
k / (k-1) * (1 - k * sigma2_item / sigma2_total)
