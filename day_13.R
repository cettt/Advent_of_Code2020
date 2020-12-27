data13 <- read.table("Input/day13.txt", sep = "")[,1]

#part1---------
bus_id <- as.integer(strsplit(gsub("x", "", data13[2]), ",+")[[1]])
t_next_bus <- (floor(as.integer(data13[1]) / bus_id) + 1) * bus_id

bus_id[which.min(t_next_bus)] * (min(t_next_bus) - as.integer(data13[1]))

#part2--------------
t_off <- which(strsplit(data13[2], ",")[[1]] != "x") - 1L #time offset
res <- 1:2*bus_id[1]

for (i in seq_along(bus_id)) {
  bus_times <- seq.int(from = res[1], by = diff(res), length.out = 1e4)
  res <- bus_times[which((bus_times + t_off[i]) %% bus_id[i] == 0)[1:2]]
}

print(res[1], 16)
