data12 <- read.table("Input/day12.txt")[,1]

navigate <- function(wp, type) { #type = 'xy' for part1 and 'wp' for part2
  xy <- 0*1i
  move <- setNames(c(1i, 1, -1i, -1), c("N", "E", "S", "W"))

  for (k in seq_along(data12)) {
    dir <- gsub("\\d", "", data12[k])
    n <- as.integer(gsub("\\D", "", data12[k]))

    if (dir %in% c("L", "R")) wp <- wp * (1i^(ifelse(dir == "R", -n, n) / 90))
    if (dir == "F") xy <- xy + n * wp

    hlp <- get(type)
    if (dir %in% names(move)) hlp <- hlp + n*move[dir]
    assign(type, hlp)
  }
  abs(Re(xy)) + abs(Im(xy))
}

# part1--------
navigate(wp = 1 + 0*1i, "xy")

# part2--------
navigate(wp = 10 + 1*1i, "wp")
