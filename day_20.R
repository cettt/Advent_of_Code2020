library(tidyverse)

rotate_m <- function(m) {
  k <- dim(m)[1]
  matrix(m[t(matrix(1:k^2, nr = k)) %*% diag(k)[,k:1]], nrow = k)
}

create_all_m <- function(m) {
  lst(m1 = as.matrix(m), m2 = rotate_m(m1), m3 = rotate_m(m2), m4 = rotate_m(m3),
      m5 = m1[, nrow(m1):1], m6 = m2[, nrow(m1):1], m7 = m3[, nrow(m1):1],
      m8 = m4[, nrow(m1):1]) %>% tibble(m = .)
}

make_bound_id <- function(m) {
  list(b_u = m[1, ], b_r = m[, ncol(m)], b_d = m[nrow(m), ], b_l = m[, 1]) %>%
    lapply(function(z) sum((z == "#") * 2^((nrow(m)-1):0))) %>%
    as_tibble()
}

data20 <- read_table("Input/day20.txt", col_names = "x", col_types = "c") %>%
  mutate(tile = cumsum(grepl("Tile", x))) %>%
  group_by(tile) %>%
  mutate(tile = parse_number(first(x))) %>%
  slice(-1) %>%
  {separate(., x, into = letters[seq_len(nchar(.$x[1]))], sep = seq_len(nchar(.$x[1])))} %>%
  nest(m = -tile) %>%
  ungroup() %>%
  mutate(m = map(m, create_all_m)) %>%
  unnest(m) %>%
  mutate(b = map(m, make_bound_id), id = 1:n()) %>%
  unnest(b)

tile_bound <- group_by(data20, tile) %>%
  summarise(b = unique(c(b_u, b_r, b_d, b_l)), .groups = "drop")

tile_neigbours <- distinct(data20, tile) %>%
  mutate(
    nei = map(tile, ~filter(tile_bound, tile == .x) %>% semi_join(tile_bound, ., by = "b") %>%
                filter(tile != .x) %>% distinct(tile)),
    n_nei = map_int(nei, nrow)
  )
#part1--------
filter(tile_neigbours, n_nei == 2) %>% pull(tile) %>% prod() %>% print(16)

#part2---------
n <- sqrt(nrow(distinct(data20, tile)))
immat2 <- matrix(0, nc = n, nr = n)

#backtracking function------
find_tile <- function(i, j) {

  if (!(i == 1 & j == 1)) {
    if (i > 1) .bd <- filter(data20, id == immat2[i - 1, j]) %>% pull(b_d)
    if (j > 1) .br <- filter(data20, id == immat2[i, j - 1]) %>% pull(b_r)

    all_id <- data20 %>%
      {`if`(i > 1, filter(., b_u == .bd), .)} %>%
      {`if`(j > 1, filter(., b_l == .br), .)} %>%
      anti_join(filter(data20, id %in% as.numeric(immat2)), by = "tile") %>%
      filter(id > immat2[i, j]) %>% pull(id)

  } else {
    all_id <- tile_neigbours %>% filter(n_nei == 2) %>%
      semi_join(data20, ., by = "tile") %>%
      filter(id > immat2[i, j]) %>% pull(id)
  }

  if (length(all_id) == 0) {immat2[i, j] <<- 0; return(FALSE)}

  immat2[i, j] <<- all_id[1]; return(TRUE)
}


k <- 1
while (k <= n^2) {
  i <- floor((k - 1) / n) + 1
  j <- k - (i - 1) * n
  k <- if (find_tile(i, j)) k + 1 else k - 1
}

n2 <- nrow(data20$m[[1]]) - 2
immat <- matrix(".", nc = n2 * n, nr = n2 * n)
for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    immat[((i-1)*n2 + 1):(i*n2), ((j-1)*n2 + 1):(j*n2)] <-
      filter(data20, id == immat2[i, j])$m[[1]][1:n2 + 1, 1:n2 + 1]
  }
}

mon <- read.table("Input/day20_monster.txt", comment.char = "", sep = ";")[,1] %>%
  {tibble(mon = str_pad(., 20, "right", " "))} %>%
  separate(mon, into = letters[1:20], sep = 1:20) %>%
  as.matrix() %>%
  {which(. == "#")}

check_im <- function(im) {
  counter <- 0
  for (i in 1:(nrow(im)- 2)) {
    for (j in 1:(ncol(im) - 19)) {
      x <- which(im[i:(i+2), j:(j+19)] == "#")
      if (all(mon %in% x)) counter <- counter  + 1
    }
  }
  return(counter)
}

create_all_m(immat) %>%
  mutate(n_monst = map_dbl(m, check_im)) %>%
  filter(n_monst > 0) %>%
  mutate(res = map2_dbl(m, n_monst, ~sum(.x == "#") - .y * length(mon))) %>%
  pull(res)
