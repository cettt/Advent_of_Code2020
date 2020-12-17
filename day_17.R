data17 <- t(apply(read.table("Input/day17.txt", comment.char = "", sep = ""),
                  1, function(x) unlist(unname(strsplit(x, split = "")))))

run_sim <- function(.dim, steps) {
  ar <- array(data = ".", dim = c(c(dim(data17), rep(1, .dim - 3)) + 2*steps, steps + 1))

  mat0 <- array(".", dim = dim(ar)[1:2]) #mat0 will be the inital xy plane
  mat0[1:dim(data17)[1] + steps, 1:dim(data17)[2] + steps] <- data17

  hlp <- c(1, cumprod(dim(ar)[-.dim]))
  ar[seq_along(mat0) + if (.dim > 3) hlp[.dim - 1] * steps else 0] <- mat0 #plug in mat0 in the right place

  make_lookup <- function(l) {#l is the index of the array (converted to 1d vector)
    co <- rep(0, .dim) #first calculatate coordinates co corresponding to l

    for (s in .dim:1) {
      co[s] <- floor((l - 1 - sum(((co - 1) * hlp)[-(1:s)])) / hlp[s]) + 1
    }

    li <- lapply(seq_len(.dim), function(s) seq(max(co[s] - 2, 0), min(co[s], dim(ar)[s] - 1)))
    as.matrix(expand.grid(li)) %*% hlp + 1 #find indices of neighbour cells
  }

  update_map <- function(l, themap) {
    z <- themap[l]
    nac <- sum(themap[lookup[[l]]] == "#")
    if (l <= prod(dim(ar)[-.dim])) {#count active cubes outside the boundary twice
      nac <- nac + sum(themap[lookup[[l]][lookup[[l]] > prod(dim(ar)[-.dim])]] == "#")
    }
    if (z == "#" & (nac < 3 | nac > 4)) "." else if (z == "." & nac == 3) "#" else z
  }

  lookup <- sapply(seq_along(ar),  make_lookup)
  map <- as.character(ar)

  for (s in seq_len(steps)) map <- sapply(seq_along(map), update_map, map)

  sum(map == "#")*2 - sum(map[seq_len(prod(dim(ar)[-.dim]))] == "#")
}

#part 1-------
run_sim(3, 6) #under 1 second

#part 2-------
run_sim(4, 6) #about 10 seconds

#used symmetry principle along the last dimension (i.e z for .dim == 3, w for .dim == 4):
# therefore in line 28: if we are at the boundary wrt to the last dimension,
#  we have to co all active cubes which are not at the boundary twice.
#Finally, in line 38 we count all active cubes twice and subtract the once at the boundary.