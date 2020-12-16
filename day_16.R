data16 <- read.table("Input/day16.txt", sep = ";")[,1]

fie_fun <- function(x) {#creates valdi values for a given field
  z <- as.numeric(strsplit(sub("^[^:]+: ", "", x), c("( or )|(\\-)"))[[1]])
  union(seq(z[1], z[2], by = 1), seq(z[3], z[4], by = 1))
}

fie_range <- lapply(data16[cumsum(data16 == "your ticket:") == 0], fie_fun)
all_ti <- t(sapply(strsplit(data16[grepl("^\\d", data16)], ","), as.numeric)) #all rows which start with a digit

#part1--------
sum(as.numeric(all_ti)[! as.numeric(all_ti) %in% Reduce(union, fie_range)])

#part 2------
val_ti <- all_ti[apply(all_ti, 1, function(x) all(x %in% Reduce(union, fie_range))), ]

check_field <- function(pos) {
  which(sapply(fie_range, function(y) all(val_ti[, pos] %in% y)))
}

pos2field_list <- lapply(seq_len(ncol(val_ti)), check_field)

fields_found <- NULL
res <- rep(0, ncol(val_ti))

for (i in seq_len(ncol(val_ti))) {
  .pos <- which(sapply(pos2field_list, function(x) length(x) == i))
  res[.pos] <- setdiff(pos2field_list[[.pos]], fields_found)
  fields_found <- c(fields_found, res[.pos])
}

print(prod(val_ti[1, which(res %in% which(grepl("departure" , data16)))]), 16)
