# FINAL VERSION CODE


# Sample Size
Sample_Size <- function(alpha, beta, delta, theta) {
  n <- (qnorm(1 - alpha) - qnorm(beta))^2 * theta * (1 - theta) / delta ^ 2
  return(n)
}




# Calculate Type I and Type II error.
Errors <- function(a0, b0, n1, n2, lambda, gamma, theta) {
  
  #Set number of simulations.
  M1 <- 1e4
  
  #Simulate the number of successes for stage 1.
  y1 <- rbinom(M1, n1, theta)
  
  # Get our posterior parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(theta, a1, b1)
  
  # Threshold of determine progression, based on the decision rule.
  C1 <-  1 - lambda * (n1 / n2) ^ gamma
  
  # Return number of simulations where threshold is exceeded for stage 1.
  M2 <- sum(fut1 > C1)
  
  # Simulate the number of successes for stage 2 given stage 1 was
  # successful.
  y2 <- rbinom(M2, n2, theta)
  
  # Get our posterior parameters at stage 2.
  a2 <- a0 + y2
  b2 <- b0 + n2 - y2
  
  # Probability of Futility.
  fut2 <- pbeta(theta, a2, b2)
  
  # Threshold o determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma
  
  # Return number of successes where threshold is exceeded for stage
  # 2, i.e. probability of type I error.
  
  return(c(sum(fut1 > C1) / M1, sum(fut2 > C2) / M2))
}



  
# Probability of y1 given theta0
Prob_y1 <- function(y1, n1, theta) {
  choose(n1, y1) * (theta ^ y1) * (1 - theta) ^ (n1 - y1) 
}


prob_y1(10, 30, 0.5)

gamma <- seq(0.01, 1, 0.1)
lambda <- seq(0.01, 1, 0.1)
df <- expand.grid(gamma = gamma, lambda = lambda)


Expected_N <- function(lambda, gamma, n1, n2) {
  
}
