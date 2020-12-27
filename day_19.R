data19 <- read.table("Input/day19.txt", sep = ";")[, 1]

idx <- as.numeric(gsub("^(\\d+).*", "\\1", data19[grepl("^\\d", data19)]))
rules <- rep("", max(idx) + 1) #rules pos is shifted by one: pos 1 is rule 0 etc.
rules[idx + 1] <- gsub("^\\d+: ", "", data19[grepl("^\\d", data19)])
words <- data19[!grepl("^\\d", data19)]

make_regex <- function(rule) {
  if (!grepl("\\d", rule)) return(rule)
  if (grepl("^\\d+$", rule)) return(make_regex(rules[as.numeric(rule) + 1]))

  y <- strsplit(rule, " ")[[1]]
  y[grepl("\\d", y)] <- sapply(y[grepl("\\d", y)], make_regex)
  gsub("\\(([^\\d])\\)", "\\1", paste0("(", paste0(y, collapse = ")("), ")"))
}

# part1----------
sum(grepl(paste0("^", make_regex(rules[1]), "$"), words))

#part2---------
r31 <- paste0("(", make_regex(rules[32]), ")$") #the regex in question for part 2 is: r31 m times...
r42 <- paste0("^(", make_regex(rules[43]), ")") # followed by r31 n times r42 n times

check_word <- function(word) {
  if (!grepl(r31, word)) return(FALSE)
  while (grepl(r31, word)) word <- sub(r42, "", sub(r31, "", word))

  grepl(paste0(r42, "+$"), word)
}

sum(sapply(words, check_word))
