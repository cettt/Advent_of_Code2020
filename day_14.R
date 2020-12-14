data14 <- read.table("Input/day14.txt", col.names = "x", sep = ";")[,1]
dec2bin <- function(z) as.numeric(rev(intToBits(as.numeric(z))))

#part1----------
pos_vec <- NULL
val_vec <- NULL

for (i in seq_along(data14)) {
  if (grepl("mask", data14[i])) {
    mask <- strsplit(sub("mask = ", "", data14[i]), "")[[1]]
  } else {
    pos_vec <- c(pos_vec, as.integer(sub("mem.(\\d+).*", "\\1", data14[i])))
    val <- c(rep("0", 4), dec2bin(sub("[^=]*= (.*)$", "\\1", data14[i])))
    val[which(mask != "X")] <- mask[which(mask != "X")]
    val_vec <- c(val_vec, sum((val == "1") * 2^(35:0)))
  }
}

print(sum(aggregate(val_vec, by = list(pos_vec), FUN = tail, 1)[,2]), 16)

#part2----------
pos_vec <- NULL
val_vec <- NULL

for (i in seq_along(data14)) {
  if (grepl("mask", data14[i])) {
    mask <- strsplit(sub("mask = ", "", data14[i]), "")[[1]]
  } else {
    val <- as.integer(sub("[^=]*= (.*)$", "\\1", data14[i]))
    pos <- c(rep("0", 4), dec2bin(sub("mem.(\\d+).*", "\\1", data14[i])))
    pos[which(mask != "0")] <- mask[which(mask != "0")]
    pos_i <- unlist(sapply(0:sum(mask == "X"),
                           function(z) combn(2^(36 - which(pos == "X")), z, sum)))
    pos_vec <- c(pos_vec, sum((pos == "1") * 2^(35:0)) + pos_i)
    val_vec <- c(val_vec, rep(val, length(pos_i)))
  }
}

print(sum(aggregate(val_vec, by = list(pos_vec), FUN = tail, 1)[,2]), 16)
