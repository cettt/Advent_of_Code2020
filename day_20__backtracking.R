check_left <- function(i, j, .id2) {

  if (j == 1) return(TRUE)
  if (immat2[i, j-1] == 0) return(TRUE)

  left_b <- x %>% filter(id2 == immat2[i, j-1]) %>%
    select(b) %>% {unlist(.)[2]}
  this_b <- x %>% filter(id2 == .id2) %>%
    select(b) %>% {unlist(.)[4]}

  return(left_b == this_b)

}

check_right <- function(i, j, .id2) {

  if (j == n) return(TRUE)
  if (immat2[i, j+1] == 0) return(TRUE)

  right_b <- x %>% filter(id2 == immat2[i, j+1]) %>%
    select(b) %>% {unlist(.)[4]}
  this_b <- x %>% filter(id2 == .id2) %>%
    select(b) %>% {unlist(.)[2]}

  return(right_b == this_b)

}

check_top <- function(i, j, .id2) {

  if (i == 1) return(TRUE)
  if (immat2[i-1, j] == 0) return(TRUE)

  top_b <- x %>% filter(id2 == immat2[i-1, j]) %>%
    select(b) %>% {unlist(.)[3]}
  this_b <- x %>% filter(id2 == .id2) %>%
    select(b) %>% {unlist(.)[1]}

  return(top_b == this_b)

}

check_bot <- function(i, j, .id2) {

  if (i == n) return(TRUE)
  if (immat2[i+1, j] == 0) return(TRUE)

  bot_b <- x %>% filter(id2 == immat2[i+1, j]) %>%
    select(b) %>% {unlist(.)[1]}
  this_b <- x %>% filter(id2 == .id2) %>%
    select(b) %>% {unlist(.)[3]}

  return(bot_b == this_b)

}

check_cell <- function(i, j, .id2) {
  # all(check_top(i, j, .id2), check_right(i, j, .id2), check_bot(i, j, .id2), check_left(i, j, .id2))
  all(check_top(i, j, .id2), check_left(i, j, .id2)) # we fill the matrix left to right top to botom
}

solve_cell <- function(i, j) {

  cur_id <- immat2[i, j]
  if (i %in% c(1, n) & j %in% c(1, n)) {
    nnei <- 2
  } else if (xor(i %in% c(1, n), j %in% c(1, n))) {
    nnei <- 3
  } else nnei <- 4

  if (!(i == 1 & j == 1)) {
    neigh <- NULL
    if (i > 1) neigh <- c(neigh, immat2[i - 1, j])
    if (j > 1) neigh <- c(neigh, immat2[i, j - 1])

    all_id <-
      y %>% filter(id2 %in% neigh) %>% select(nei) %>% .[[1]] %>%
      Reduce(intersect, .) %>%
      {filter(y, id %in% ., n_nei == nnei, id2 > immat2[i, j])} %>%
      pull(id2)

  } else {
    all_id <- y %>% filter(n_nei == 2, id2 > immat2[i, j]) %>% pull(id2)
  }


  if (immat2[i, j] == max(all_id)) {
    immat2[i, j] <<- 0
    return(FALSE)
  }

  for (s in all_id) {
    if (check_cell(i, j, s)) {
      immat2[i, j] <<- s
      return(TRUE)
    }
  }

  immat2[i, j] <<- 0
  return(FALSE)
}