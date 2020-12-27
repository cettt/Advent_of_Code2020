data06 <- c("" , read.table("Input/day06.txt", blank.lines.skip = FALSE)[,1])
id <- cumsum(data06 == "")
res <- aggregate(data06, list(id), function(x) table(strsplit(paste0(x[x!= ""], collapse = ""), "")[1]))

# part1----------------
sum(sapply(res[,2], length))

# part2----------------
sum(sapply(seq_along(res[,2]), function(i) sum(res[i, 2][[1]] == sum(id == i) - 1)))
