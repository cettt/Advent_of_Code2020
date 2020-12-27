data04 <- read.table("Input/day04.txt", sep = ";", fill = TRUE, comment.char = "",
                blank.lines.skip = FALSE, col.names = "x")

x <- aggregate(data04, by = list(cumsum(data04[,1] == "") + 1),
                      FUN = function(x) paste0(x, collapse =" "))$x
#part 1------
idx <- grepl("eyr", x) & grepl("byr", x) & grepl("iyr", x) & grepl("ecl", x) &
  grepl("hcl", x) & grepl("hgt", x) & grepl("pid", x)
sum(idx)

#part 2------
y <- data.frame(x = x[idx])

y$eyr <- as.numeric(sub(".*eyr:(\\d+).*", "\\1", y$x))
y$byr <- as.numeric(sub(".*byr:(\\d+).*", "\\1", y$x))
y$iyr <- as.numeric(sub(".*iyr:(\\d+).*", "\\1", y$x))
y$ecl <- sub(".*ecl:([a-z]+).*", "\\1", y$x)
y$hcl <- sub(".*hcl:(#[a-f0-9]{6}).*", "\\1", y$x)
y$pid <- sub(".*pid:(\\d+).*", "\\1", y$x)
y$hgt <- as.numeric(sub(".*hgt:(\\d+).*", "\\1", y$x))
y$hgt_u <- sub(".*hgt:\\d+([^ ]+).*", "\\1", y$x)

sum(y$byr >= 1920 & y$byr <= 2002 & y$iyr >= 2010 & y$iyr <= 2020 & y$eyr >= 2020 &
    y$eyr <= 2030 & y$ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth") &
    nchar(y$hcl) == 7 & nchar(y$pid) == 9 & (
      (y$hgt_u == "cm" & y$hgt >= 150 & y$hgt <= 193) |
        (y$hgt_u == "in" & y$hgt >= 59 & y$hgt <= 76)
    ))
