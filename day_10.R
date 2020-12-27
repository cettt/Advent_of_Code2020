data10 <- read.table("Input/day10.txt")[,1]

#part1-----------
x <- sort(c(0, data10, max(data10) + 3))
prod(table(diff(x)))

#part2------------
opt_x <- x[-c(1, which(diff(x) == 3), which(diff(x) == 3) + 1, length(x))]
print(prod(pmin(2^table(cumsum(c(0, diff(opt_x) > 2))), 7)), 16)

#x all adapters
#opt_x all optional adapters: these do not have distance three to either of their 'neighbours'
#cumsum(c(0, diff(setdiff(x, fix_x)) > 2) assigns every optional adapter to a group
#table(...)  these groups are of sizes 1, 2 and 3
#2^table(...) number of combinations within each group
#pmin(..., 7) in groups with size three it is not feasible to choose non of the optional adapters.
#prod(....) take the product