data24 <- read.table("Input/day24.txt")[,1]


z <- data24[1]
f <- function(z) {
  z <- strsplit(gsub("([ew])", "\\1-", z), "-")[[1]]
  y <- (sum(grepl("n", z)) - sum(grepl("s", z))) * 1/2
  x <- sum(z == "e") - sum(z == "w") + 1/2*sum(grepl("[ns]e", z)) - 1/2*sum(grepl("[ns]w", z))
  return(c(x,y))
}

#part 1------------
sum(table(apply(sapply(data24, f), 2, paste, collapse = ",")) %% 2 == 1)

#part 2-----------
library(dplyr)
map_black <- count(as.data.frame(unname(t(sapply(data24, f)))), V1, V2) %>%
  filter(n %% 2 == 1) %>% rename(x = V1, y = V2)

whole_map <- rbind(
  expand.grid(x = seq(-114, 115), y = seq(-65, 66)),
  expand.grid(x = seq(-114.5, 115.5), y = seq(-65.5, 66.5))
  )

whole_map <- whole_map %>%
  semi_join(map_black, by = c("x", "y")) %>% mutate(col = 1) %>%
  bind_rows(anti_join(whole_map, map_black, by = c("x", "y")) %>% mutate(col = 0)) %>%
  mutate(id = 1:n())


whole_map %>% count(col)

make_lookup <- function(k) {
  x0 <- whole_map[k, 1]
  y0 <- whole_map[k, 2]
  filter(whole_map, abs(x - x0) <= 1, abs(y - y0) <= 1/2) %>% pull(id)
}

lookup <- lapply(seq_along(whole_map[,1]), make_lookup)

update_map <- function(k, themap) {
  z <- themap[k]
  nbl <- sum(themap[lookup[[k]]])
  if (z == 1 & (nbl == 1 | nbl > 3)) 0 else if (z == 0 & nbl == 2) 1 else z
}

map <- whole_map[,3]

for (s in 1:100) map <- sapply(seq_along(map), update_map, map)

sum(map)
