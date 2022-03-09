# FINAL VERSION CODE

# Calculate Type I and Type II error.
Errors <- function(a0, b0, n1, n2, lambda, gamma, theta) {
  
  #Set number of simulations.
  M <- 1e4
  
  #Simulate the number of successes for stage 1.
  y1 <- rbinom(M, n1, theta)
  
  # Get our posterior parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(theta, a1, b1)
  
  # Threshold o determine progression, based on the decision rule.
  C1 <-  1 - lambda * (n1 / n2) ^ gamma
  
  # Return number of simulations where threshold is exceeded for stage 1.
  s1 <- sum(fut1 > C1)
  
  # Simulate the number of successes for stage 2 given stage 1 was
  # successful.
  y2 <- rbinom(s1, n2, theta)
  
  # Get our posterior parameters at stage 2.
  a2 <- a1 + y2
  b2 <- b1 + n2 - y2
  
  # Probability of Futility.
  fut2 <- pbeta(theta, a2, b2)
  
  # Threshold o determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma
  
  # Return number of successes where threshold is exceeded for stage
  # 2, i.e. probability of type I error.
  s2 <- sum(fut2 > C2)
  
  return(c(s1 / M, s2 / M))
  
}
 
  
  
# Probability of y1 given theta0
prob_y1 <- function(y1, n1, theta) {
  choose(n1, y1) * (theta ^ y1) * (1 - theta) ^ (n1 - y1) 
}

prob_y1(10, 30, 0.5)  
