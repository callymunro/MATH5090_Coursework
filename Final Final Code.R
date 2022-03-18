# Code to accompany an evaluation of the BOP2 trial design.



# Initial calculation of one stage sample size.
Sample_Size <- function(alpha, beta, delta, theta) {
  n <- (qnorm(1 - alpha) - qnorm(beta))^2 * theta * (1 - theta) / delta ^ 2
  return(n)
}

Sample_Size(0.05, 0.2, 0.2, 0.5)
# This gives an estimated (rounded) sample size for the first stage as n1 = 40.

# Function to work out the Type I and II errors.
Errors_General <- function(x, a0, b0, n1, n2, theta) {
  
  lambda <- x[1]
  gamma <- x[2]
  
  #Set the number of simulations.
  M1 <- 1e4
  
  #Simulate the number of successes for stage 1 using our given theta.
  y1 <- rbinom(M1, n1, theta)
  
  # Get our posterior Beta(a1, b1) parameters.
  a1 <- a0 + y1
  b1 <- b0 + n1 - y1
  
  # Probability of Futility.
  fut1 <- pbeta(0.5, a1, b1)
  
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
  fut2 <- pbeta(0.5, a2, b2)
  
  # Threshold to determine progression, based on the decision rule.
  C2 <-  1 - lambda * (n2 / n2) ^ gamma
  
  # Type one error when generated under the null hypothesis.
  # Type two error when generated under the alternate hypothesis.
  return(sum(fut1 <= C1 & fut2 <= C2) / M1)
}
  

# Create a data frame of all possible pairs of gamma and lambda to be considered.
# We have restricted both to [0, 1].
gamma <- seq(0.01, 1, 0.05)
lambda <- seq(0.01, 1, 0.05)
df <- expand.grid(lambda = lambda, gamma = gamma)


ptm <- proc.time()

# Calculating the operating characteristics for each pair of tuning parameters.
Type_I <- apply(df, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 40, n2 = 80, theta = 0.5)
Type_II <- 1 - (apply(df, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 40, n2 = 80, theta = 0.7))

Type_I_II <- cbind(Type_I, Type_II)

proc.time() - ptm

# Reduce pairs of gamma and lambda to those which satisfy restrictions on
# Type I AND Type II error.
acceptable_pairs <- which(Type_I_II[, 1] <= 0.05 & Type_I_II[, 2] <= 0.2)

# To a dataframe.
df_acceptable_pairs <- df[acceptable_pairs, ]



# Consider the probability of y1 given theta0.
Prob_y1 <- function(y1, n1, theta) {
  choose(n1, y1) * (theta ^ y1) * (1 - theta) ^ (n1 - y1) 
}

# Function to calculate the expected sample size given a pair of decision rule parameters.
Expected_N <- function(x, n1, n2, theta) {
  gamma <- x[1]
  lambda <- x[2]
  
  # Threshold to determine progression, based on the decision
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

# Calculate the expected sample size for each acceptable pair (lambda, gamma).
Sample_Size <- apply(df_acceptable_pairs, 1, Expected_N, n1 = 40, n2 = 80, theta = 0.5)

# Round each sample size to minimum feasible.
Round_Up <- ceiling(Sample_Size)

#Calculate the power for each acceptable pair to allow comparisons between those
# with the same expected sample size.
Power <- apply(df_acceptable_pairs, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 40, n2 = 80, theta = 0.70)

# View a table with the acceptable pairs, their expected sample size and the power.
Full_Data <- cbind(df_acceptable_pairs, Round_Up, Power)



