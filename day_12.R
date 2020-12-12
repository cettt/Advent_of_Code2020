data12 <- tidyr::separate(readr::read_table("Input/day12.txt", col_names = "x"),
                          x, into = c("dir", "n"), sep = 1, convert = TRUE)

navigate <- function(wp, type) { #type = 'xy' for part1 and 'wp' for part2
  xy <- c(0, 0)
  ro_f <- function(k) { #k is the number ccw turns by 90 degrees
    round(matrix(c(cos( k / 2 * pi), sin(k / 2 * pi),
                   sin(-k / 2 * pi), cos(k / 2 * pi)), 2, 2), 2)
  }

  for (i in seq_len(nrow(data12))) {
    dir = data12$dir[i]
    n = data12$n[i]

    if (dir %in% c("L", "R")) wp <- as.numeric(ro_f(ifelse(dir == "R", -1, 1) * n / 90) %*% wp)
    if (dir == "F") xy <- xy + n * wp
    hlp <- get(type)
    if (dir == "N") hlp[2] = hlp[2] + n
    if (dir == "E") hlp[1] = hlp[1] + n
    if (dir == "S") hlp[2] = hlp[2] - n
    if (dir == "W") hlp[1] = hlp[1] - n
    assign(type, hlp)
  }
  sum(abs(xy))
}

# part1--------
navigate(c(1, 0), "xy")

# part2--------
navigate(c(10, 1), "wp")
