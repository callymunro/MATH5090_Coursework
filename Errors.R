# Errors Code (Efficient??)


Errors_General <- function(a0, b0, n1, n2, lambda, gamma, theta) {
  
  #Set the number of simulations.
  M1 <- 1e5

  #Simulate the number of successes for stage 1 using our given theta.
  y1 <- rbinom(M1, n1, theta)
  
  # Get our posterior Beta(a1, b1) parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(theta, a1, b1)
  
  # Threshold of determine progression, based on the decision rule.
  C1 <-  1 - lambda * (n1 / n2) ^ gamma
  
  # Return number of simulations where we continue to stage 2.
  M2 <- sum(fut1 <= C1)
  
  # Simulate the number of successes for stage 2 given stage 1 was
  # successful.
  y2 <- rbinom(M2, n2 - n1, theta)
  
  # Get our posterior Beta(a2, b2) parameters.
  a2 <- a0 + y1 + y2
  b2 <- b0 + n2 - y1 - y2
  
  # Probability of Futility.
  fut2 <- pbeta(theta, a2, b2)
  
  # Threshold to determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma

  return(sum(fut2 > C2) / M1)
}

# To calculate the type I and II errors, we use the above function with theta = 0.5 for 
# Type I error, then 1 - function with theta = 0.7.
TypeI_II <- function(a0, b0, n1, n2, lambda, gamma, theta_null, theta_alt) {
  return(c(Errors_General(a0, b0, n1, n2, lambda, gamma, theta_null),
           1 - Errors_General(a0, b0, n1, n2, lambda, gamma, theta_alt)))
}