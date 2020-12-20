library(tidyverse)
data20 <- read_table("Input/day20.txt", col_names = "x")

r100 <- as.numeric(t(matrix(1:100, nr = 10)) %*% diag(10)[,10:1])
rotate_m <- function(m) matrix(m[r100], nr = 10)
make_b <- function(m) {
  list(b1 = m[1, ], b2 = m[, 10], b3 = m[10, ], b4 = m[, 1]) %>%
    sapply(function(z) sum((z == "#") * 2^(9:0)))
}

x <- data20 %>%
  mutate(gr = cumsum(grepl("Tile", x))) %>%
  group_by(gr) %>%
  mutate(id = parse_number(first(x))) %>%
  slice(-1) %>%
  separate(x, into = as.character(0:9), sep = 1:10) %>%
  nest(im = `0`:`9`) %>%
  ungroup() %>%
  mutate(
    m_1_0 = map(im, ~unname(as.matrix(.x))),
    m_2_0 = map(m_1_0, rotate_m),
    m_3_0 = map(m_2_0, rotate_m),
    m_4_0 = map(m_3_0, rotate_m),
    m_1_x = map(m_1_0, ~.x[, 10:1]),
    m_2_x = map(m_2_0, ~.x[, 10:1]),
    m_3_x = map(m_3_0, ~.x[, 10:1]),
    m_4_x = map(m_4_0, ~.x[, 10:1]),
    m_1_y = map(m_1_0, ~.x[10:1, ]),
    m_2_y = map(m_2_0, ~.x[10:1, ]),
    m_3_y = map(m_3_0, ~.x[10:1, ]),
    m_4_y = map(m_4_0, ~.x[10:1, ]),
    m_1_xy = map(m_1_x, ~.x[10:1, ]),
    m_2_xy = map(m_2_x, ~.x[10:1, ]),
    m_3_xy = map(m_3_x, ~.x[10:1, ]),
    m_4_xy = map(m_4_x, ~.x[10:1, ])
  ) %>%
  select(-im, -gr) %>%
  gather(hlp, m, -id) %>%
  separate(hlp, into = c("dummy", "rot", "flip")) %>%
  select(-dummy) %>%
  mutate(b = map(m, make_b)) %>%
  arrange(id, rot, flip) %>%
  mutate(id2 = 1:n())


show_matching <- function(.b, .id) {
  x %>% filter(id != .id) %>%
    select(id, b) %>%
    filter(map_lgl(b, ~any(.x %in% .b))) %>%
    distinct(id) %>%
    pull(id)
}

y <- x %>%
  mutate(
    nei = map2(b, id, show_matching),
    n_nei = map_int(nei, length)
  )

#part1--------
y %>% filter(n_nei == 2) %>% distinct(id) %>% pull(id) %>% prod() %>% print(16)
#part2---------
#backtracking
n <- sqrt(nrow(distinct(y, id)))
immat2 <- matrix(0, nc = n, nr = n)
k <- 1
source("backtracking.R")

while (k <= n^2) {
  i <- floor((k - 1) / n) + 1
  j <- k - (i-1)*n
  if (solve_cell(i, j)) {
    k = k + 1
  } else {
    k = k - 1
  }
}

tibble(id2 = as.numeric(immat2)) %>%
  left_join(y %>% select(id, id2),  by ="id2") %>%
  pull(id) %>%
  matrix(nc = n)

immat <- matrix(".", nrow = (10*n), ncol = (10*n))
for (i in seq_len(n)) {
  for (j in seq_len(n)) {
    immat[((i-1)*10 + 1):(i*10), ((j-1)*10 + 1):(j*10)] <- y %>%
      filter(id2 == immat2[i, j]) %>% select(m) %>% unlist() %>% matrix(nc = 10)
  }
}

immat_nob <- immat[seq_len(10*n) %% 10 > 1, seq_len(10*n) %% 10 > 1] #image without borders

mon <- read.table("Input/day20_monster.txt", comment.char = "", sep = ";")[,1] %>%
  {tibble(mon = str_pad(., 20, "right", " "))} %>%
  separate(mon, into = letters[1:20], sep = 1:20) %>%
  as.matrix() %>%
  {which(. == "#")}

im <- immat_nob

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

k <- dim(immat_nob)[1]
rotate_m2 <- function(m) {
  rmat <- as.numeric(t(matrix(1:k^2, nr = k)) %*% diag(k)[,k:1])
  matrix(m[rmat], nr = k)
}

tibble(m_1_0 = list(immat_nob)) %>%
  mutate(
    m_2_0 = map(m_1_0, rotate_m2),
    m_3_0 = map(m_2_0, rotate_m2),
    m_4_0 = map(m_3_0, rotate_m2),
    m_1_x = map(m_1_0, ~.x[, k:1]),
    m_2_x = map(m_2_0, ~.x[, k:1]),
    m_3_x = map(m_3_0, ~.x[, k:1]),
    m_4_x = map(m_4_0, ~.x[, k:1]),
    m_1_y = map(m_1_0, ~.x[k:1, ]),
    m_2_y = map(m_2_0, ~.x[k:1, ]),
    m_3_y = map(m_3_0, ~.x[k:1, ]),
    m_4_y = map(m_4_0, ~.x[k:1, ]),
    m_1_xy = map(m_1_x, ~.x[k:1, ]),
    m_2_xy = map(m_2_x, ~.x[k:1, ]),
    m_3_xy = map(m_3_x, ~.x[k:1, ]),
    m_4_xy = map(m_4_x, ~.x[k:1, ])
  ) %>%
  gather(hlp, m) %>%
  separate(hlp, into = c("dummy", "rot", "flip")) %>%
  select(-dummy) %>%
  mutate(n_monst = map_dbl(m, check_im)) %>%
  filter(n_monst > 0) %>%
  slice(1) %>%
  mutate(res = map2_dbl(m, n_monst, ~sum(.x == "#") - .y*length(mon))) %>%
  pull(res)
