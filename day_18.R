data18 <- read.table("Input/day18.txt", sep = ";")[,1]

solve_hw <- function(z, type = "") {

  if (type == "advanced") z <- gsub("(\\d+ \\+ \\d+)", "(\\1)", z)

  if (!grepl("\\(", z)) {
    y <- unname(strsplit(z, " ")[[1]])
    res <- as.numeric(y[1])

    for (i in 2 * seq_len((length(y) - 1) / 2)) {
      res <- (if (y[i] == "+") `+` else `*`)(res, as.numeric(y[i + 1]))
    }
    return(res)
  }
  pa <- stringr::str_extract_all(z, patter = "\\([^\\(\\)]*\\)")[[1]]
  pa_val <- sapply(gsub("\\((.*)\\)", "\\1", pa), solve_hw, type = "")

  for (val in pa_val) z <- sub("\\([^\\(\\)x]*\\)", paste0("x", val), z)
  #everytime we calculate the value of a paranthesis we add an x
  solve_hw(gsub("x", "", z), type = type) #here we remove the x again
}

#part1------
print(sum(sapply(data18, solve_hw)), 16)

#part2-----
print(sum(sapply(data18, solve_hw, type = "advanced")), 16)
