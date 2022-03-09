# MATH5090M
# Coursework1

samplesize <- function(alpha, beta, sigma, delta) {
  n <- (dnorm(1 - alpha) - dnorm(beta))^2 * (2 * sigma^2) / delta ^ 2
  
  return(n)
}

samplesize(0.05, 0.4, 10, 0.2)

# Sample Size for both arms

# Treatment difference would be the comparison of the theta values
# so 0.5 vs 0.8.

# could consider binary endpoint for two arm trial of an absolute
# difference of 0.3.


# _______________________________________________________
# FINDING THE MINIMUM EXPECTED N

# Set the number of simulations
M <- 10^4

# Create an empty vector to store simulated Ns
Ns <- rep(NA, M)
for (i in 1:M) {
  # Simulate theta from its prior, and then stage 1 data
  # conditional on this theta.
  theta <- rbeta(1, 0.5, 0.5)
  y1 <- rbinom(1, n1, theta)
  
  # Get posterior Beta(a1, b1) parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of futility.
  fut1 <- pbeta(theta, a1, b1)
  
  # Threshold to determine progression, based on the decision
  # rule.
  C1 <- 1 - lambda * (n1 / n2) ^ gamma
}