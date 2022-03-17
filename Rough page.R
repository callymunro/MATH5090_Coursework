# Evaluation

gamma <- seq(0.01, 1, 0.05)
lambda <- seq(0.01, 1, 0.05)
df <- expand.grid(gamma = gamma, lambda = lambda)


ptm <- proc.time()

#ATTEMPT TO LIST TYPE I AND II ERRORS
Type_I <- apply(df, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 100, n2 = 200, theta = 0.5)
Type_II <- 1 - (apply(df, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 100, n2 = 200, theta = 0.7))

Type_I_II <- cbind(Type_I, Type_II)

proc.time() - ptm

# Reduce pairs of gamma and lambda to those with acceptable Type I and Type II error
acceptable_pairs <- which(Type_I_II[, 1] <= 0.05 & Type_I_II[, 2] <= 0.2)

# Pick out the acceptable pairs of lambda and gamma that satisfy error
# restrictions.

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

Sample_Size <- apply(df_acceptable_pairs, 1, Expected_N, n1 = 100, n2 = 200, theta = 0.5)

Round_Up <- ceiling(Sample_Size)

Power <- apply(df_acceptable_pairs, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 100, n2 = 200, theta = 0.7)

Full_Data <- cbind(df_acceptable_pairs, Round_Up, Power)

Full_Data