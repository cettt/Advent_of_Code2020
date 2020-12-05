data05 <- sapply(strsplit(read.table("Input/day05.txt")[,1], split = ""),
                 function(x) sum(x %in% c("B", "R") * 2^(9:0)))
#part1-------
max(data05)

#part2--------
sort(data05)[which(diff(sort(data05)) != 1)] + 1
