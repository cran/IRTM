### ----------------------------
### Test: continuous IRT-M model
### ----------------------------

set.seed(6889)

# Simulate a simple continuous-measurement model
N <- 100   # respondents
K <- 3     # items
d <- 1     # latent dimension

# True latent trait
theta_true <- rnorm(N, 0, 1)

# True loadings + intercepts
lambda_true <- matrix(c(0.8, -1.2, 0.5), ncol = 1)
b_true <- c(0.4, -0.2, 1.0)

# Generate continuous outcomes
Y <- matrix(NA, N, K)
for (k in 1:K) {
  mu <- theta_true * lambda_true[k] - b_true[k]
  Y[, k] <- rnorm(N, mean = mu, sd = 1)
}

# Run continuous IRTM
res <- M_constrained_irt_continuous(
  Y,
  d = d,
  nburn = 2000,
  nsamp = 4000,
  thin = 5,
  learn_Sigma = TRUE,
  learn_Omega = TRUE,
  display_progress = FALSE
)

# Examine results
str(res)

# Posterior mean estimates
theta_est <- apply(res$theta, c(1, 2), mean)
lambda_est <- apply(res$lambda, c(1, 2), mean)
b_est <- rowMeans(res$b)

# Print comparison
cat("True Lambda:\n")
print(lambda_true)

cat("\nEstimated Lambda:\n")
print(lambda_est)

cat("\nTrue b:\n")
print(b_true)

cat("\nEstimated b:\n")
print(b_est)

# Simple correlation check on theta recovery
cat("\nCorrelation between true and estimated theta:\n")
print(cor(theta_true, theta_est))
