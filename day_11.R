data11 <- apply(read.table("Input/day11.txt", comment.char = "", sep = ""),
                1, function(x) unlist(unname(strsplit(x, split = ""))))
cdata11 <- as.character(data11)

make_lookup <- function(k, lu_fun) {
  if (cdata11[k] == ".") return(NULL)
  j <- floor((k-1) / dim(data11)[1]) + 1
  i <- k - (j-1) * dim(data11)[1]

  res <- unique(c(
    lu_fun(which(row(data11) - col(data11) == i - j), k), #first diagonal
    lu_fun(which(row(data11) + col(data11) == i + j), k), #second diagonal
    lu_fun(seq_len(dim(data11)[1]) + (j-1)*dim(data11)[1], k), #fixed col
    lu_fun(i + (seq_len(dim(data11)[2]) - 1) * dim(data11)[1], k) #fixed row
  ))
  res[!is.na(res)]
}

update_map <- function(k, themap, lookup, th) {
  z <- themap[k]
  noc <- sum(themap[lookup[[k]]] == "#")
  if (z == "L" & noc == 0) "#" else if (z == "#" & noc > th) "L" else z
}

simulate_seat <- function(lookup_fun, th) {

  lookup <- lapply(seq_len(prod(dim(data11))), make_lookup, lu_fun = lookup_fun)
  oldmap <- 0
  newmap <- cdata11

  while (!identical(newmap, oldmap)) {
    oldmap <- newmap
    newmap <- sapply(seq_along(oldmap), update_map, oldmap, lookup, th = th)
  }
  sum(as.character(oldmap) == "#")
}

# part1------------
lookup_fun1 <- function(k_vec, k) k_vec[which(k_vec == k) + -1:1] #include k

simulate_seat(lookup_fun1, 4) #about 3 seconds

#part2------
lookup_fun2 <- function(k_vec, k) {
  k_vec <- k_vec[cdata11[k_vec] != "."] #remove "."
  k_vec[which(k_vec == k) + -1:1]
}

simulate_seat(lookup_fun2, 5) #about 3 seconds
