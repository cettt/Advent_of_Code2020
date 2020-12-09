data09 <- read.table("Input/day09.txt")[,1]

# part 1-----------
checksum <- function(i) {
  x <- tail(data09[1:(i-1)], 25)
  data09[i] %in% as.numeric(outer(x, x, "+"))[-(1+(0:24)*26)] #at the end exclude the diagonal
}

idx <- which(!sapply(26:length(data09), checksum)) + 25
data09[idx]

# part 2-----------
find_weak <- function(i) {
  x <- cumsum(data09[i:(idx-1)]) == data09[idx]
  if (any(x)) sum(range(data09[i:(i + which(x) - 1)])) else NULL
}

unlist(sapply(seq_len(idx - 1), find_weak))
