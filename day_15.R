data15 <- as.numeric(read.table("Input/day15.txt", sep = ",")[1, ])

run_day15 <- function(n, input) {
  tab_l <- rep(0, n) #tab_l[x] = t means that the last time x has been said was at time t
  tab_l[input + 1] <- seq_along(input)
  y <- 0 #first number after the input sequence is zero

  for (turn in (length(input) + 1):n) {
    y2 <- turn - tab_l[y + 1] #y2 candidate for the next number in the sequence (after y)
    tab_l[y + 1] <- turn #update tab_l
    y <- y2 * (y2 != turn) #if y2 == turn then y had never been said before in which case y <- 0
  }
  return(which(                                             tab_l == n) - 1)
}

#part 1--------
run_day15(2020, data15)

#part 2---------
run_day15(30000000, data15) #8 seconds
