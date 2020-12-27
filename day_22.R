data22 <- read.table("Input/day22.txt", sep = ";")[, 1]

x <- as.integer(data22[cumsum(grepl("Player", data22)) == 1][-1])
y <- as.integer(data22[cumsum(grepl("Player", data22)) == 2][-1])

play_combat <- function(x, y, recursion = TRUE, score = FALSE) {
  k <- 1L
  lookup <- vector("list", 1000L)

  while (length(x) * length(y)) {
    if (any(sapply(lookup[seq_len(k)], identical, list(x, y)))) return(TRUE)
    lookup[[k]] <- list(x, y)
    k <- k + 1L

    x1 <- x[1]; y1 <- y[1]
    x <- x[-1]; y <- y[-1]

    if (length(x) >= x1 & length(y) >= y1 & recursion) {
      x2 <- x[seq_len(x1)]; y2 <- y[seq_len(y1)]
      win <- if (max(x2) > max(y2)) TRUE else play_combat(x2, y2)
    } else win <- x1 > y1

    if (win) x <- c(x, x1, y1) else y <- c(y, y1, x1)
  }
  if (!score) length(x) > 0 else sum(rev(c(x, y)) * seq_along(c(x, y)))
}

#part 1-------
play_combat(x, y, recursion = FALSE, score = TRUE)

#part 2-------
play_combat(x, y, recursion = TRUE, score = TRUE)
