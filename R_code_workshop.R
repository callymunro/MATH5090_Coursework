# Workshop
# 
# rbinom(1, n1, theta0)
# 
# y1 <- sim data()
# 
# d1 <- decision(y1, n1, delta, lambda, (a0, b0?))
# 
# if(d1 = true) {
#   y2 <- sim data(n2)
#   d2 <- decision(y1, n1, y2, n2, delta, lambda)
#   # if decision is true, generate new data?
#   # this leaves us with two decisions we have recorded.
#   # d1 isnt really relevant for our type 1 error rate?
#   # if we wanted our code to give us our overall result
#   # to our trial, what would we want to return?
# }




# if we want to say we are only interested in using this design 
# for a specific prior distribution, we could have a0 and b0
# included as parameters

# potentially describe the prior as weakly informative.
# could keep a0 b0 as .5 .5 and justify by saying it is a weakly
# informative prior, though you might want to say
# what are the operating characteristics, or the trial designs,
# if we allow different priors.

# say from previous studies in this area, we would expect something
# like a probability of .3 as a response, unlikely it would be
# ineffective, very unlikely to be completely effective.

# could describe our prior to have this skewed curve.
# however, we could be putting our own opinion in the data.
# is this ok to justify with previous data in the study.
# say it is something brand new, less justification to (specify a0, b0?)

# most used as prior is some form of weakly informative prior 
# distribution.

# could maybe look at a different prior and see what this 
# do to the type one error,  change our sample size, ...

# intuitively, suggests that if we start with some information,
# we would need a lower sample size to get the same clarity of data.

# defining type I and type II errors need to be defined through
# the BOP2 design

#_________________________________________________________
# OPTIMISING THE SAMPLE SIZE

# General comments on how to optimise.

# say we want to minimize something whilst searching over values of 
# gamma and lambda subject to some constraint, in our case
# alpha \le 0.05
# beta \le 0.1

# need to decide on a metric for the left over (gamma, lambda)
# pairs that match constraints, but minimise the expected 
# sample size.

# grid seach, discard pairs that don't fit constraints,
# work out expected SS
# pick the smallest.

# could take a set of candidate n1 n2 values, can fix those
# and find the best gamma and lambda, then compare expected sample size.

# so minimise including n1, n2, delta, gamma

# slow: monte carlo
# fast: vectorisation?

#________________________________________________________
# What could we do other than optimisation?

# Duncan says it is ok to not do a formal optimisation, but
# could consider some comparison.
# Consider a candidate set of a few designs.
# figure out the metrics for each (lambda gamma alpha beta)

# Have a guess for n1, n2.

#________________________________________________________
#Working through a simulation.

# Looking at code in 4.2, (evaluate_design_exact)
# This example doesn't show the null, it is working
# out the expected sample size from a bayesian consideraation.
# Averaging over a distibution?

# From our Bayesian perspective, we would need to include a theta argument
# Where would this come in to our calc
# Threshold would be the same
# y_1s would be the same
# progression decision would be fine (stops is the posterior, evaluated at 0.5 which is the 
# probability of futility threshold)

# Would need to include theta in y_1_probs
# y_1_probs <-  prob_y1(
# i.e. what is the the probability of having y1=20 when n1=30?
# This is on Pr(y1 = 20 | theta = theta0)
# Going back to the function prob_y1, (much more complicated
# case in the notes to average over.)
# Conditional on these, we only need to work out p(y1|theta)p(\theta)
# which is just
# prob_y1 <- function(y1, n1, theta) {
# (n1 choose y1) theta^(y1) (1-theta)^(n1-y1)
# }

# binomial as the prior.

# can use all the code in exact_fun (whatever that is)
# but change the prob_y1 to have the prior just in the one case.

# Could do a much smaller grid search, or pick out a few
# hypothetical designs.
# of the ones we have considered, these are the best ones.


# Why would we use a probability of futility to progress?
# Just what the authors proposed, this could be something to 
# review.
# In the literature, they have justified this in comparison.

# Endpoint is satisfying constraints with a small sample size.