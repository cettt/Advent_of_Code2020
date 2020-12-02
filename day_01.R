x <- read.table("Input/day01.txt")[,1]

# part1---------
prod(x[which(outer(x, x, "+") == 2020, arr.ind = TRUE)[1,]])

# part2---------
prod(x[which(outer(x, outer(x, x, "+"), "+") == 2020, arr.ind = TRUE)[1,]])