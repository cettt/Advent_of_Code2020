data03 <- apply(read.table("Input/day03.txt", comment.char = "", sep = ""),
                1, function(x) unlist(unname(strsplit(x, split = ""))))

check_slope <- function(z) {
  table(data03[(z[1]*(col(data03)-1) - z[2]*(row(data03)-1)) %% nrow(data03) == 0 &
                 (col(data03) - 1) %% z[2] == 0])[1]
}

# part1-----------
check_slope(c(3, 1))

# part2 ----------
prod(apply(data.frame(x = c(1, 3, 5, 7, 1), y = c(1, 1, 1, 1, 2)), 1, check_slope))
