# Suárez Pérez Juan Pablo
# 15 / 11 / 2022

# 1.33

# Cards are drawn from a standard deck, 
# with replacement, until an ace appears.
# Simulate the mean and variance of the number
# of cards required.

main <- function() {
  # Replicate the experiment n times 
  n <- 100000
  res <- replicate(n, get_ace_from_deck())
  cards_requiered_mean <- mean(res)
  cards_required_var <- var(res)
  print("The results:")
  cat("Mean:", cards_requiered_mean, ".\n")
  cat("Var:", cards_required_var, ".")
}

get_ace_from_deck <- function() {
  # Consider a standard deck, with the next
  # Terminology:
    # h: hearts
    # d: diamonds
    # c: clubs
    # s: spades
  # and
    # a: ace
    # j: jack
    # q: queen
    # k: king
  deck <- c("a_h", "2_h", "3_h", "4_h", "5_h", "6_h", "7_h", "8_h", "9_h",
            "10_h", "j_h", "q_h", "k_h",
            "a_d", "2_d", "3_d", "4_d", "5_d", "6_d", "7_d", "8_d", "9_d",
            "10_d", "j_d", "q_d", "k_d",
            "a_c", "2_c", "3_c", "4_c", "5_c", "6_c", "7_c", "8_c", "9_c",
            "10_c", "j_c", "q_c", "k_c",
            "a_s", "2_s", "3_s", "4_s", "5_s", "6_s", "7_s", "8_s", "9_s",
            "10_s", "j_s", "q_s", "k_s")
  deck_size <- length(deck)
  picked_cards <- character()
  ace <- F
  i <- 1
  while (ace == F) {
    picked_cards[i] <- sample(deck, 1, prob = rep(c(1 / deck_size), deck_size))
    if (substr(picked_cards[i], 1, 1) == "a")
      ace <- T
    else
      i <- i + 1
  }
  
  return(i)
}

main()