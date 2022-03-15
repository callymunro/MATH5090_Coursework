# FINAL VERSION CODE


# Sample Size
Sample_Size <- function(alpha, beta, delta, theta) {
  n <- (qnorm(1 - alpha) - qnorm(beta))^2 * theta * (1 - theta) / delta ^ 2
  return(n)
}

# This gives an estimated sample size for the first arm as n1 = 40



# Calculate Type I and Type II error.
# We use a balanced design where 2n1 = n2 = 80

Type_I <- function(a0, b0, n1, n2, lambda, gamma, theta_null) {
  
  #Set number of simulations.
  M1 <- 1e4
  
  #Simulate the number of successes for stage 1.
  y1 <- rbinom(M1, n1, theta_null)
  
  # Get our posterior parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(theta_null, a1, b1)
  
  # Threshold of determine progression, based on the decision rule.
  C1 <-  1 - lambda * (n1 / n2) ^ gamma
  
  # Return number of simulations where threshold is exceeded for stage 1.
  M2 <- sum(fut1 > C1)
  
  # Simulate the number of successes for stage 2 given stage 1 was
  # successful.
  y2 <- rbinom(M2, n2, theta_null)
  
  # Get our posterior parameters at stage 2.
  a2 <- a0 + y2
  b2 <- b0 + n2 - y2
  
  # Probability of Futility.
  fut2 <- pbeta(theta_null, a2, b2)
  
  # Threshold o determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma
  
  # Return number of successes where threshold is exceeded for stage
  # 2, i.e. probability of type I error.
  
  return(sum(fut2 > C2) / M1)

}

# Type_II error would be 1-Type_I (with theta = 0.7)
Type_II <- function(a0, b0, n1, n2, lambda, gamma, theta_alt) {
  
  #Set number of simulations.
  M1 <- 1e4
  
  #Simulate the number of successes for stage 1.
  y1 <- rbinom(M1, n1, theta_alt)
  
  # Get our posterior parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(theta_alt, a1, b1)
  
  # Threshold of determine progression, based on the decision rule.
  C1 <-  1 - lambda * (n1 / n2) ^ gamma
  
  # Return number of simulations where threshold is exceeded for stage 1.
  M2 <- sum(fut1 > C1)
  
  # Simulate the number of successes for stage 2 given stage 1 was
  # successful.
  y2 <- rbinom(M2, n2, theta_alt)
  
  # Get our posterior parameters at stage 2.
  a2 <- a0 + y2
  b2 <- b0 + n2 - y2
  
  # Probability of Futility.
  fut2 <- pbeta(theta_alt, a2, b2)
  
  # Threshold o determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma
  
  # Return number of successes where threshold is exceeded for stage
  # 2, i.e. probability of type I error.
  
  return(sum(fut2 < C2) / M1)
}




# Create a data frame of all possible pairs of gamma and lambda
gamma <- seq(0.01, 1, 0.05)
lambda <- seq(0.01, 1, 0.05)
df <- expand.grid(gamma = gamma, lambda = lambda)

# Change grid to allow for a grid search
# This should calculate the Type I and II error for each pair
gridsearch <- function(x, n1, n2, theta, a0, b0) {
  
  # Separate gamma and lambda as columns of the data frame
  gamma <- x[1]
  lambda <- x[2]
  
  TypeI_II <- function(a0, b0, n1, n2, lambda, gamma, theta_null, theta_alt) {
    theta_null <- 0.5
    theta_alt <- 0.7
    return(c(Errors_General(a0, b0, n1, n2, lambda, gamma, theta_null),
             Errors_General(a0, b0, n1, n2, lambda, gamma, theta_alt)))
  }
  # #Set number of simulations.
  # M1 <- 1e4
  # 
  # #Simulate the number of successes for stage 1.
  # y1 <- rbinom(M1, n1, theta)
  # 
  # # Get our posterior parameters.
  # a1 <- a0 + y1
  # b1 <- b0 + n1 - y1
  # 
  # # Probability of Futility.
  # fut1 <- pbeta(0.5, a1, b1)
  # 
  # # Threshold of determine progression, based on the decision rule.
  # C1 <-  1 - lambda * (n1 / n2) ^ gamma
  # 
  # # Return number of simulations where threshold is exceeded for stage 1.
  # M2 <- sum(fut1 < C1)
  # 
  # # Simulate the number of successes for stage 2 given stage 1 was
  # # successful.
  # y2 <- rbinom(M2, n2 - n1, theta)
  # 
  # # Get our posterior parameters at stage 2.
  # a2 <- a0 + y1 + y2
  # b2 <- b0 + n2 - y2 - y1
  # 
  # # Probability of Futility.
  # fut2 <- pbeta(0.5, a2, b2)
  # 
  # # Threshold to determine progression, based on the decision rule.
  # C2 <-  1 - lambda * (n2 / n2) ^ gamma
  # 
  # # Return number of successes where threshold is exceeded for stage
  # # 2, i.e. probability of type I error.
  # return(c(sum(fut1 > C1) / M1, sum(fut2 > C2) / M2))

}

fun <- apply(df, 1, gridsearch, n1 = 40, n2 = 80, theta = 0.5, a0 = 0.5, b0 = 0.5)
rownames(fun)

# Reduce pairs of gamma and lambda to those with acceptable Type I and Type II error
acceptable_pairs <- which(fun[1, ] <= 0.05 & fun[2, ] <= 0.2)

# Collect these pairs into a new dataframe
df_acceptable_pairs <- df[acceptable_pairs, ]

# Consider the probability of y1 given theta0
Prob_y1 <- function(y1, n1, theta) {
  choose(n1, y1) * (theta ^ y1) * (1 - theta) ^ (n1 - y1) 
}


Expected_N <- function(x, n1, n2, theta) {
  gamma <- x[1]
  lambda <- x[2]
  
  # Threshold to determing progression, based on the decision
  # rule.
  C1 <- 1 - lambda * (n1 / n2) ^ gamma
  
  # Vector of possible stage 1 outcomes.
  y_1s <- 0:n1
  
  # Vector of corresponding progression decisions.
  stops <- pbeta(0.5, y_1s + 0.5, n1 - y_1s + 0.5) < C1
  
  # For each outcome, calculate its probability.
  y_1_probs <- Prob_y1(y_1s, n1, theta)
  
  sum(n1 * stops * y_1_probs + n2 * (!stops) * y_1_probs)
}
