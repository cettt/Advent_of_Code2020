data08 <- read.table("Input/day08.txt", sep = ";")[, 1]

runcode <- function(mycode) {
  check <- rep(0, length(mycode))
  i <- 1
  acc <- 0

  while (TRUE) {
    check[i] <- check[i] + 1
    if (check[i] < 2) {
      if (grepl("ac", mycode[i])) acc <- acc + readr::parse_number(mycode[i])
      i <- i + if_else(grepl("j", mycode[i]), readr::parse_number(mycode[i]), 1)

      if (i > length(mycode)) {
        print(acc)
        return(TRUE)
      } # for part 2
    } else return(acc)
  }
}

# part1-----------
runcode(data08)

# part2----------
for (j in which(!grepl("acc", data08))) {
  x <- data08
  x[j] <- paste(if (grepl("j", x[j])) "" else "j", readr::parse_number(x[j]))
  y <- runcode(x)
  if (isTRUE(y)) break
}
