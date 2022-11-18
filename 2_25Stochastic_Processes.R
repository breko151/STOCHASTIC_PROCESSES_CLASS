# Suárez Pérez Juan Pablo
# 15 / 11 / 2022


# 2.25

# The behavior of dolphins in the presence of tour boats in Patagonia, 
# Argentina is studied in Dans et al. (2012). A Markov chain model is developed, 
# with state space consisting of five primary dolphin activities (socializing,
# traveling, miling, feeding, and resting). The following transition matrix is 
# obtained.
# Use technology to estimate the long-term distribution of dolphin activity.

main <- function() {
  mat_dolphin <- matrix(c(0.84, 0.11, 0.01, 0.04, 0.00,
                          0.03, 0.80, 0.04, 0.10, 0.03,
                          0.01, 0.15, 0.70, 0.07, 0.07,
                          0.03, 0.19, 0.02, 0.75, 0.01,
                          0.03, 0.09, 0.05, 0.00, 0.83), ncol = 5, byrow = T)
  margin <- 10 ** -8
  i <- 1
  mat_n_1 <- mat_dolphin
  number_entries <- dim(mat_dolphin)[1] * dim(mat_dolphin)[2]
  flag <- T
  while (flag) {
    mat_n <- mat_n_1 %*% mat_dolphin
    mat_aux <- mat_n - mat_n_1
    count <- length(mat_aux[abs(mat_aux) < margin])
    if (count == number_entries) {
      flag <- F
    }
    else {
      i <- i + 1
      mat_n_1 <- mat_n
    }
  }
  cat("In", i, "iterations, the matrix transition converges to the following matrix:\n")
  print(mat_n)
}

main()

