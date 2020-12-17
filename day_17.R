data17 <- t(apply(read.table("Input/day17.txt", comment.char = "", sep = ""),
                  1, function(x) unlist(unname(strsplit(x, split = "")))))

run_sim <- function(.dim, steps) {
  ar <- array(data = rep(".", prod(dim(data17) + 2*steps) * (2*steps + 1)),
              dim = c(dim(data17) + 2*steps, rep(2*steps + 1, .dim - 2)))
  mat0 <- array(rep(".", prod(dim(ar)[1:2])), dim = dim(ar)[1:2])
  mat0[1:dim(data17)[1] + steps, 1:dim(data17)[2] + steps] <- data17

  hlp <- c(1, cumprod(dim(ar)))
  co2idx <- function(co) sum((co - 1) * hlp[-(.dim + 1)]) + 1

  ar[co2idx(c(1, 1, rep(steps + 1, .dim - 2))):
       co2idx(c(dim(data17) + 2*steps, rep(steps + 1, .dim - 2)))] <- mat0


  make_lookup <- function(l) {
    co <- rep(0, .dim) #first calculatate coordinates co corresponding to l

    for (s in .dim:1) {
      co[s] <- floor((l - 1 - sum(((co - 1)*hlp[-(.dim + 1)])[-(1:s)])) / hlp[s]) + 1
    }

    li <- lapply(seq_len(.dim), function(s) unique(pmax(pmin(co[s] + 0:2 - 1, dim(ar)[s]), 1)))
    setdiff(apply(expand.grid(li), 1, co2idx), l)
  }

  update_map <- function(l, themap) {
    z <- themap[l]
    nac <- sum(themap[lookup[[l]]] == "#")
    if (z == "#" & !nac %in% c(2,3)) "." else if (z == "." & nac == 3) "#" else z
  }

  lookup <- sapply(seq_along(ar),  make_lookup)
  mymap <- as.character(ar)

  for (s in seq_len(steps)) mymap <- sapply(seq_along(ar), update_map, mymap)

  sum(mymap == "#")
}

#part 1-------
run_sim(3, 6)

#part 2-------
run_sim(4, 6)
