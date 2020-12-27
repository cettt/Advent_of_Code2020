data23 <- as.integer(strsplit(as.character(read.table("Input/day23.txt")[,1]), "")[[1]])

play_cups <- function(rounds, input = data23, n = 1e6L, part1 = TRUE) {
  x <- seq_len(n)
  x[seq_along(input)] <- input

  #cp1[t] is the cup after cup t which, i.e. cup + 1
  cp1 <- seq_len(n)
  cp1[x[seq_len(n - 1)]] <- x[-1L]
  cp1[x[n]] <- x[1L]
  cup0 <- x[1L]
  for (s in seq_len(rounds)) {
    cup1 <- cp1[cup0]
    cup2 <- cp1[cup1]
    cup3 <- cp1[cup2]

    dest <- if (cup0 != 1L) cup0 - 1L else n #this is faster than using %%
    while (dest %in% c(cup1, cup2, cup3)) dest <- if (dest != 1L) dest - 1L else n

    hlp <- cp1[cup3]
    cp1[cup0] <- hlp
    cup0 <- hlp
    cp1[cup3] <- cp1[dest]
    cp1[dest] <- cup1
  }
  if (part1) cp1 else prod(cp1[1], cp1[cp1[1]])
}

#part1------
res <- play_cups(rounds = 100, data23, n = length(data23), part1 = TRUE)
f <- function(x, n) if (n == 1) x else x[f(x, n - 1)]
paste0(sapply(2:9, function(z) f(res, n = z)[which(res == 1)]), collapse = "")

#part 2---------
print(play_cups(rounds = 1e7, data23, n = 1e6, part1 = FALSE), 16) #about 16 seconds
