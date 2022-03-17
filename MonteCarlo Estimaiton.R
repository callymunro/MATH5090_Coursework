evaluate_design <- function(x, n1, n2) {
  
  # Estimate the expected sample size of a design defined by its 
  # decision rule parameters (lambda, gamma) and sample size 
  # parameters (n1, n2), along with its standard error.
  
  # Set the number of simulations.
  M <- 10^4
  # Create an empty vector to store simulated NS.
  Ns <- rep(NA, M)
  for (i in 1:M) {
    # Simulate theta from its prior, and then the stage 1 data conditional
    # on this theta.
    theta <- rbeta(1, 0.5, 0.5)
    y1 <- rbinom(1, n1, theta)
    
    # Get posterior Beta(a1, b1) parameters.
    a1 <- 0.5 + y1
    b1 <- 0.5 + n1 - y1
    
    # Probability of futility.
    fut1 <- pbeta(0.5, a1, b1)
    
    # Threshold to determine progression, based on the decision rule.
    C1 <- 1 - lambda * (n1 / n2)^gamma
    
    # Note the final total sample size and store in the vector Ns.
    if (fut1 > C1) {
      Ns[i] <- n1
    } else {
      Ns[i] <- n2
    }
  }
  
  
  # Return the estimated expected sample size.
  return(mean(Ns))
}


Sample_Size <- apply(df_acceptable_pairs, 1, evaluate_design, n1 = 40, n2 = 80)

Round_Up <- ceiling(Sample_Size)

#Calculate the power for each acceptable pair to allow comparisons between those
# with the same expected sample size.
Power <- apply(df_acceptable_pairs, 1, Errors_General, a0 = 0.5, b0 = 0.5, n1 = 40, n2 = 80, theta = 0.70)

# View a table with the acceptable pairs, their expected sample size and the power.
Full_Data <- cbind(df_acceptable_pairs, Round_Up, Power)

