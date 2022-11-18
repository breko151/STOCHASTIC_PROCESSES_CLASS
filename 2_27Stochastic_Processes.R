# Suárez Pérez Juan Pablo
# 15 / 11 / 2022


# 2.27

# See gamblersruin.R. Simulate gambler's ruin for a gambler with initial 
# stake $2, playing a fair game.

# a) Estimate the probability the probability that the gambler is ruined before 
#    he wins $5

# Simulate Gambles Ruin
gamble <- function(k, n, p, goal) {
  stake <- k
  stake_historic <- integer()
  reach_goal <- FALSE
  i <- 1
  
  while (stake > 0 & stake < n) {
    bet <- sample(c(-1, 1), 1, prob = c(1 - p, p))
    stake <- stake + bet
    stake_historic[i] <- stake 
    if (stake == goal)
      reach_goal = TRUE
    i <- i + 1
  }
  
  if (stake == 0) 
    return(list(1, reach_goal)) 
  else 
    return(list(0, reach_goal))
} 

k <- 2
n <-  1000 # Let's assign a big number to represent that the rival has so much money 
p <- 0.50
goal <- 5
trials <- 1000

res <- replicate(trials, gamble(k, n, p, goal))
res_not_reach_goal <- res[2, res[1,] == 1 & res[2,] == FALSE]
prob_not_reach_goal <- length(res_not_reach_goal) / trials
cat("The probability that the gambler is ruined before reaching the goal of $",
    goal, "is", prob_not_reach_goal, "\nwith p =", p, ", i.e. a fair game")
# b) Construct the transition matrix for the associated Markov chain. Estimate
#    the desired probability in a) by taking high matrix powers
trans_matrix_gamblers_ruin <- matrix(c(1, 0, 0, 0, 0, 0,
                                       1 - p, 0, p, 0, 0, 0,
                                       0, 1 - p, 0, p, 0, 0,
                                       0, 0, 1 - p, 0, p, 0,
                                       0, 0, 0, 1 - p, 0, p,
                                       0, 0, 0, 0, 0, 1),
                                     ncol = 6, byrow = TRUE)
error <- 10 ** -8
i <- 1
A_n_minus_1 <- trans_matrix_gamblers_ruin
number_entries <- dim(trans_matrix_gamblers_ruin)[1] * dim(trans_matrix_gamblers_ruin)[2]
flag <- TRUE
while (flag) {
  A_n <- A_n_minus_1 %*% trans_matrix_gamblers_ruin
  aux_mat <- A_n - A_n_minus_1
  aux_count <- length(aux_mat[abs(aux_mat) < error])
  if (aux_count == number_entries) {
    flag <- FALSE
  }
  else {
    i <- i + 1
    A_n_minus_1 <- A_n
  }
}
df_trans_matrix_gamblers_ruin = data.frame(round(A_n, 2))
rownames(df_trans_matrix_gamblers_ruin) <- c("wealthy_0", "wealthy_1",
                                             "wealthy_2", "wealthy_3",
                                             "wealthy_4", "wealthy_5")
colnames(df_trans_matrix_gamblers_ruin) <- c("wealthy_0", "wealthy_1",
                                             "wealthy_2", "wealthy_3",
                                             "wealthy_4", "wealthy_5")
cat("\nIn", i, "iterations, the matrix transition converges to the following matrix:\n")
print(df_trans_matrix_gamblers_ruin)
# c) Compare your results with the exact probability.