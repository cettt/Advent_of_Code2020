data22 <- read.table("Input/day22.txt", sep = ";")[, 1]

x <- as.numeric(data22[cumsum(grepl("Player", data22)) == 1][-1])
y <- as.numeric(data22[cumsum(grepl("Player", data22)) == 2][-1])

play_combat <- function(x, y, recursion = TRUE, score = FALSE) {
  k <- 0
  lookup <- list()

  while (length(x) * length(y) != 0) {
    k <- k + 1
    if (any(sapply(lookup, identical, list(x = x, y = y)))) return(TRUE)
    lookup[[k]] <- list(x = x, y = y)

    x1 <- x[1]; y1 <- y[1]
    x <- x[-1]; y <- y[-1]

    if (length(x) >= x1 & length(y) >= y1 & recursion) {
      x2 <- x[seq_len(x1)]; y2 <- y[seq_len(y1)]
      win <- if (max(x2) > max(y2)) TRUE else play_combat(x2, y2)
    } else win <- x1 > y1

    if (win) x <- c(x, x1, y1) else y <- c(y, y1, x1)
  }

  if (score) sum(rev(x) * seq_along(x)) + sum(rev(y) * seq_along(y)) else length(x) > 0
}

#part 1-------
play_combat(x, y, recursion = FALSE, score = TRUE)

#part 2-------
play_combat(x, y, recursion = TRUE, score = TRUE)
