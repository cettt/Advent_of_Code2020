data02 <- read.table("Input/day02.txt")

p <- gsub(":", "", data02[, 2])
n <- sapply(data02[, 1], function(z) as.numeric(strsplit(z, "-")[[1]]))
x <- sapply(seq_along(p), function(i) table(strsplit(data02[i, 3], "")[[1]])[p[i]])
y <- sapply(seq_along(p), function(i) substring(data02[i, 3], n[, i], n[, i]))

#part1-------
sum(sapply(seq_along(p), function(i) x[i] >= n[1, i] & x[i] <= n[2, i]), na.rm = 1)

#part2--------
sum(sapply(seq_along(p), function(i) xor(p[i] == y[1, i], p[i] == y[2, i])))

