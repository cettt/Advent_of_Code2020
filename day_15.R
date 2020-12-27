data15 <- as.integer(read.table("Input/day15.txt", sep = ",")[1, ])

run_day15 <- function(n, input) {
  tab_t <- vector("integer", n) #tab_t[x] = t means that the last time x-1 has been said was at time t
  tab_t[input + 1L] <- seq_along(input)
  y <- 0L #first number after the input sequence is zero

  for (turn in seq.int(length(input) + 1L, n)) {
    y2 <- turn - tab_t[y + 1L] #y2 candidate for the next number in the sequence (after y)
    tab_t[y + 1L] <- turn #update tab_t
    y <- y2 * (y2 != turn) #if y2 == turn then y had never been said before in which case y <- 0
  }
  return(which(tab_t == n) - 1L)
}

#part 1--------
run_day15(2020L, data15)

#part 2---------
run_day15(30000000L, data15) #8 seconds
