# Reed Frost Model
# Suárez Pérez Juan Pablo 
# 21 / 11 / 2022

reed_frost_next_step <- function(size, infected, z) {
  p <- 1 - (1 - z) ** infected
  new_infected <- rbinom(n = 1, size = size, prob =  p)
  new_size <- size - new_infected
  state_next <- c(S = new_size, I = new_infected)
  return(state_next) 
}
 
evolution_reed_frost <- function(size, infected, z, steps) {
  evolution <- matrix(nrow = steps + 1, ncol = 3)
  evolution[1,1] <- 0
  evolution[1,2] <- size
  evolution[1,3] <- infected
  colnames(evolution) <- c("time", "S", "I")
  for (step in 1:steps) {
    evolution[step + 1, 1] <- step
    results <- reed_frost_next_step(evolution[step, 2], evolution[step, 3], z)
    evolution[step + 1, 2] <- results[1]
    evolution[step + 1, 3] <- results[2]
  }
  return(evolution)
  
}
results <- evolution_reed_frost(400, 3, 0.004, 20)
plot(I~time, data=results, type="l")