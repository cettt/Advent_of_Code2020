library(tidyverse)

data07 <- read_table("Input/day07.txt", col_names = FALSE, col_types = "c") %>%
  separate(1, into = c("outer", "x"), sep = " contain ") %>%
  separate(x, into = letters[1:4], sep = ", ", fill = "right") %>%
  gather(dummy, inner, -outer, na.rm = TRUE) %>%
  mutate(
    n = parse_number(sub("no other bag", "0", inner)),
    across(c(inner, outer), ~sub("\\d ", "", sub("bags?\\.?", "bag", .))), #remove digits and plural s
  )

# part1-----------
find_outer_bags <- function(inner_bag) {

  outer_bags <- data07 %>% filter(inner %in% inner_bag) %>% pull(outer)
  if (length(outer_bags) == 0) return(NULL)
  return(unique(c(outer_bags, find_outer_bags(outer_bags))))
}

length(find_outer_bags("shiny gold bag"))

#part2----------
count_inner_bags <- function(outer_bag) {

  data07 %>% filter(outer == outer_bag) %>%
    summarise(x = sum(n + n * map_dbl(inner, count_inner_bags))) %>% # sum of empty col returns zero
    pull(x)
}

count_inner_bags("shiny gold bag")
