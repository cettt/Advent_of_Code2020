data24 <- read.table("Input/day24.txt")[,1]

find_hex <- function(path) { #complex doubled coordinates
  x <- table(factor(path, levels = c("nw", "sw", "w", "ne", "se", "e")))
  sum(x[4:6]) - sum(x[1:3]) + x[6] - x[3] + (x[1] + x[4] - x[2] - x[5])*1i
}

hex_co <- sapply(strsplit(gsub("([ew])", "\\1-", data24), "-"), find_hex)

#part1---------
sum(table(hex_co) %% 2 == 1)

#part2-------
black_hex <- as.complex(names(table(hex_co)[table(hex_co) %% 2 == 1]))

co_grid <- apply(expand.grid(
  r = seq(from = min(Re(black_hex)) - 101, to = max(Re(black_hex)) + 101),
  i = seq(from = min(Im(black_hex)) - 101, to = max(Im(black_hex)) + 101)
), 1, function(z) z[1] + z[2] * 1i)

co_grid <- co_grid[(Re(co_grid) + Im(co_grid)) %% 2 == 0]

make_lookup <- function(k) {
  z <- co_grid[k]
  k_vec <- seq(max(1, k - 140), min(length(co_grid), k + 140))
  k_vec <- k_vec[abs(Im(z - co_grid[k_vec])) <= 1]
  k_vec[abs(z - co_grid[k_vec]) <= 2]
}

update_map <- function(k, themap) {
  z <- themap[k]
  nbl <- sum(themap[lookup[[k]]])
  if (z == 1 & (nbl == 1 | nbl > 3)) 0 else if (z == 0 & nbl == 2) 1 else z
}

lookup <- lapply(seq_along(co_grid),  make_lookup)
map <- ifelse(co_grid %in% black_hex, 1, 0)

for (s in 1:100) {
  idx <- seq(max(1, min(which(map == 1)) - 140), min(length(co_grid), max(which(map == 1)) + 140))
  map[idx] <- sapply(idx, update_map, map)
}
sum(map)
