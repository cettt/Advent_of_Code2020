library(tidyverse)

data06 <- read.table("Input/day06.txt", blank.lines.skip = FALSE) %>%
  setNames("x") %>%
  mutate(id = cumsum(x == "")) %>%
  group_by(id) %>%
  summarise(
    y = map(paste0(x[x != ""], collapse = ""), str_count, pattern = letters),
    n = length(x[x != ""]), .groups = "drop",
  )

# part1----------------
summarise(data06, res = sum(map_int(y, ~length(.x[.x > 0]))))

# part2----------------
summarise(data06, res = sum(map2_int(y, n, ~length(.x[.x == .y]))))
