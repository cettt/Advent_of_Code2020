library(tidyverse)

data02 <- read.table("Input/day02.txt") %>%
  setNames(c("n", "p", "pw")) %>%
  separate(n, into = c("n1", "n2"), sep = "-", convert = TRUE) %>%
  mutate(p = sub(":", "", p))

#part1--------
data02 %>%
  filter(n1 <= str_count(pw, p), str_count(pw, p) <= n2) %>%
  nrow()

#part2----------
data02 %>%
  filter(xor(p == str_sub(pw, n1, n1), p == str_sub(pw, n2, n2))) %>%
  nrow()
