data21 <- read.table("Input/day21.txt", sep = ";")[,1]

al <- sub("\\)", "", sub("^.*contains ", "", data21))
ing <- sub(" \\(.*", "", data21)

al_list <- data.frame(al = sort(unique(unlist(strsplit(al, ", ")))), ing = NA)

while (any(is.na(al_list[, 2]))) {
  for (a in al_list[is.na(al_list[, 2]), 1]) {
    all_ing <- strsplit(ing[which(grepl(paste0("^", a), al) | grepl(paste0(" ", a), al))], " ")
    x <- setdiff(Reduce(intersect, all_ing), al_list[!is.na(al_list[,2]), 2])
    if (length(x) == 1) al_list[al_list[, 1] == a, 2] <- x
  }
}

#part1-------
sum(sapply(lapply(strsplit(ing, " "), setdiff, y = al_list[, 2]), length))

#part2------
paste0(al_list[, 2], collapse = ",")
