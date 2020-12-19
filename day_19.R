data19 <- read.table("Input/day19.txt", sep = ";")[,1]
rules <- rep("", max(idx))

idx <- as.numeric(gsub("^(\\d+).*", "\\1", data19[grepl("^[1-9]", data19)]))
rules[idx] <- gsub("^\\d+: ", "", data19[grepl("^[1-9]", data19)])


find_words <- function(rule) {
  if (!grepl("\\d", rule)) return(rule)
  if (!grepl(" ", rule)) return(find_words(rules[as.numeric(rule)]))

  y <- strsplit(strsplit(rule, " \\| ")[[1]], " ")

  res <- lapply(y, function(z) sapply(z, function(w) find_words(rules[as.numeric(w)])))
  unlist(lapply(res, function(z) {
    if (is.matrix(z)) {
      if (nrow(z) > 1) as.character(outer(z[,1], z[,2], paste0)) else  paste0(z, collapse = "")
    } else if (is.list(z)) {
      apply(expand.grid(z), 1, function(w) paste0(w, collapse = ""))
    } else paste0(z, collapse = "")
  }
  ))
}

check_word <- function(x, type = "part1") {

  x1 <- substr(x, 1, nchar(r42[1]))
  xn <- substr(x, nchar(x)- nchar(r31[1]) + 1, nchar(x))
  if (!xn %in% r31) return(FALSE)

  while (x1 %in% r42 & xn %in% r31) {#check rule 11 n times
    x <- substr(x, nchar(r42[1]) + 1, nchar(x) - nchar(r31[1]))
    x1 <- substr(x, 1, nchar(r42[1]))
    xn <- substr(x, nchar(x) - nchar(r31[1]) + 1, nchar(x))
    if (type == "part1") break
  }

  if (!x1 %in% r42) return(FALSE)

  while (x1 %in% r42) {#check rule 8 n times
    x <- substr(x, nchar(r42[1]) + 1, nchar(x))
    x1 <- substr(x, 1, nchar(r42[1]))
    if (type == "part1") break
  }
  return(x == "")
}

words <- data19[!grepl("^\\d", data19)]
r42 <- find_words(42)
r31 <- find_words(31)

# part1----------
sum(sapply(words, check_word))

#part2---------
sum(sapply(words, check_word, type = "part2"))
