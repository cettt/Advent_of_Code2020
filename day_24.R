data24 <- read.table("Input/day24.txt")[,1]

find_hex <- function(path) {
  tab <- table(factor(path, levels = c("nw", "ne", "sw", "se", "w", "e")))
  c(x = as.numeric((tab[2] + tab[4] - tab[1] - tab[3]) / 2 + tab[6]- tab[5]),
    y = (sum(tab[1:2]) - sum(tab[3:4])) / 2)
}

xy <- t(sapply(strsplit(gsub("([ew])", "\\1-", data24), "-"), find_hex))
xy <- aggregate(seq_along(xy[, 1]) ~ x + y, data = xy, function(z) length(z) %% 2)

#part1---------
sum(xy[,3])

#part2-------
x_range <- range(xy[, 1]) + c(-100, 100)
y_range <- range(xy[, 2]) + c(-50, 50)

map_mat <- rbind(expand.grid(x = seq(x_range[1], x_range[2]),
                             y = seq(round(y_range[1]), round(y_range[2]))),
                 expand.grid(x = seq(x_range[1] - 1/2, x_range[2] + 1/2),
                             y = seq(y_range[1], y_range[2])))

map_mat <- merge(map_mat, xy, all.x = TRUE)
map_mat[is.na(map_mat[, 3]), 3] <- 0

make_lookup <- function(k) {
  xy0 <- as.numeric(map_mat[k, 1:2])
  which(abs(map_mat[,1] - xy0[1]) <= 1 & abs(map_mat[,2] - xy0[2]) <= 1/2)
}

lookup <- lapply(seq_along(map_mat[,1]), make_lookup)

update_map <- function(k, themap) {
  z <- themap[k]
  nbl <- sum(themap[lookup[[k]]])
  if (z == 1 & (nbl == 1 | nbl > 3)) 0 else if (z == 0 & nbl == 2) 1 else z
}

map <- map_mat[, 3]
for (s in 1:100) map <- sapply(seq_along(map), update_map, map)
sum(map)

