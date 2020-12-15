data15 <- as.numeric(read.table("Input/day15.txt", sep = ",")[1, ])

run_day15 <- function(n, input) {
  tab_l <- rep(0, n) #contains the last time a certain number was seen
  tab_l[input + 1] <- seq_along(input)
  y <- 0

  for (turn in (length(input) + 1):n) {
    y2 <- turn - tab_l[y + 1] #y2 is x[turn]
    tab_l[y + 1] <- turn #update tab_l
    y <- y2 * (y2 != turn) #if tab_l = zero x[turn + 1] is set to zero
  }
  return(which(tab_l == n) - 1)
}

#part 1--------
run_day15(2020, data15)

#part 2---------
run_day15(30000000, data15) #8 seconds
