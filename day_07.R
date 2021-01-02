library(tidyverse)

data07 <- strsplit(gsub("bags?\\.?", "bag", readLines("Input/day07.txt")), " contain ")

bags_df <- do.call(rbind, lapply(data07, function(z) {
  y <- strsplit(z[2], ", ")[[1]]
  n <- as.integer(gsub("\\D+", "", y))
  data.frame(outer = z[1], inner = gsub("\\d ", "", y), n = n)
}))

# part1-----------
find_outer_bags <- function(inner_bag) {
  outer_bags <- subset(bags_df, inner %in% inner_bag)[, 1]
  if (length(outer_bags) == 0) return(NULL)
  return(unique(c(outer_bags, find_outer_bags(outer_bags))))
}

length(find_outer_bags("shiny gold bag"))

#part2----------
count_inner_bags <- function(outer_bag) {

  x <- subset(bags_df, outer == outer_bag)
  if (nrow(x) == 0) return(0)

  sum(sapply(seq_len(nrow(x)), function(i) x[i, 3] * (1 + count_inner_bags(x[i, 2]))), na.rm = TRUE)
}

count_inner_bags("shiny gold bag") #30899
