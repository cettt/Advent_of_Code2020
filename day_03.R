library(magrittr)

data03 <- read.table("Input/day03.txt", comment.char = "", sep = "") %>% 
  apply(1, function(x) unlist(unname(strsplit(x, split = "")))) %>%
  t()

# part1-----------
data03[1:nrow(data03), ((1:nrow(data03) - 1) * 3) %% ncol(data03) + 1] %>%
  diag() %>%
  {length(.[which(. == "#")])}

# part2 ----------
check_slope <- function(x) {
  (seq_len(ceiling(nrow(data03) / x[2])) - 1) %>% #count the number of steps to reach goal
    {data03[1 + . * x[2], 1 + (. * x[1]) %% ncol(data03)]} %>% #subset corresponding entries
    diag() %>% 
    {length(.[which(. == "#")])}
}

data.frame(x = c(1, 3, 5, 7, 1), y = c(1, 1, 1, 1, 2)) %>%
  apply(1, check_slope) %>%
  prod()
