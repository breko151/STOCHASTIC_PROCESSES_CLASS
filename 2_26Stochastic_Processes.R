# Suárez Pérez Juan Pablo
# 15 / 11 / 2022


# 2.26

# In computer security applications, a honeypot is a trap set on a network to
# detect and counteract computer hackers. Honeypot data are studied in Kimou
# et al. (2010) using Markov chains. The authors obtain honeypot data from a 
# cental database and observe attacks against four computer ports--80, 135, 
# 139, and 445--over 1 year. The ports are the states of Markov chain along
# with a state corresponding to no port is attacked. Weekly data are monitored, 
# and the port most often attacked the week is recorded. The estimated
# Markov transition matrix for weekly attacks is
p = matrix(c(0, 0, 0, 0, 1,
             0, 8/13, 3/13, 1/13, 1/13,
             1/16, 3/16, 3/8, 1/4, 1/8, 
             0, 1/11, 4/11, 5/11, 1/11,
             0, 1/8, 1/2, 1/8, 1/4), ncol = 5, byrow = T)
# a) Which are the least and most likely attacked ports after 2 weeks?
library(tidyverse)
library(ggplot2)
markov <- function(init,mat,n,labels) { 
  if (missing(labels)) labels <- 1:length(init)
  simlist <- numeric(n+1)
  states <- 1:length(init)
  simlist[1] <- sample(states,1,prob=init)
  for (i in 2:(n+1)) 
  { simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) }
  labels[simlist]
}
init = c(0, 0, 0, 0, 1)
n <- 2
labels <- c('Port 80', 'Port 135', 'Port 139', 'Port 445', 'No attack')
trials <- 1000
res <- replicate(trials, markov(mat = p, init = init,
                                n = n, labels = labels))
states_2_weeks <- res[3,]
counts <- as.data.frame(table(states_2_weeks))
print(ggplot(counts, aes(x=states_2_weeks, y=Freq, fill=states_2_weeks)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = Freq), vjust = 1.5, color = "white") +
  labs(cat("Attacked ports after 2 weeks (", trials, " trials )")))
cat("\nThe most port 139, the least port 80")
# b) Find the long-term distribution of attacked ports.
error <- 10 ** -8
i <- 1
A_n_1 <- p
number_entries <- dim(p)[1] * dim(p)[2]
flag <- TRUE
while (flag) {
  A_n <- A_n_1 %*% p
  aux_mat <- A_n - A_n_1
  aux_count <- length(aux_mat[abs(aux_mat) < error])
  if (aux_count == number_entries) {
    flag <- FALSE
  }
  else {
    i <- i + 1
    A_n_1 <- A_n
  }
}
cat("\nIn", i, "iterations, the matrix transition converges to the following matrix:\n")
print(A_n)