# Suárez Pérez Juan Pablo
# 15 / 11 / 2022


# 1.37

# Simulate the results of Exercise 1.28.
# Estimate the mean and variance of the number of accidents per day.


# 1.28

# On any day, the number of accidents on the highway has a Poisson distribution 
# with parameterA. The parameter A varies from day to day and is itself a random 
# variable. Find the mean and variance of the number of accidents per day when A 
# is uniformly distributed on (0, 3)

main <- function() {
  n <- 1000000
  a <- 0
  b <- 3
  
  set.seed(1)
  rand_lambda <- runif(n, min = a, max = b)
  accidents <- rpois(n, rand_lambda)
  
  hist(accidents, freq = F)
  
  accidents_means <- mean(accidents)
  accidents_var <- var(accidents)
  
  cat("Mean:", accidents_means, ".\n")
  cat("Variance:", accidents_var, ".")
}


main()