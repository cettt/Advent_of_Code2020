library(tidyverse)

data04 <- read_file("Input/day04.txt") %>%
  str_split(pattern = "\r\n", simplify = TRUE) %>% 
  tibble(x = as.character(.)) %>%
  mutate(id = cumsum(x == "") + 1) %>%
  group_by(id) %>%
  summarise(x = paste0(x[x != ""], collapse = " "), .groups = "drop") %>%
  separate(x, sep = "\\s+", into = letters[1:9], fill = "right") %>%
  gather(dummy, y, -id, na.rm = TRUE) %>%
  select(-dummy) %>%
  separate(y, into = c("type", "value"), sep = ":") %>%
  spread(type, value)

# part1---------
data04 %>%
  filter(across(-c("id", "cid"), ~!is.na(.x))) %>% 
  nrow()

# part2----------
data04 %>% 
  filter(
    between(parse_number(byr), 1920, 2002), 
    between(parse_number(iyr), 2010, 2020),
    between(parse_number(eyr), 2020, 2030),
    ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    grepl("^\\d{9}$", pid),
    grepl("^#[0-9,a-f]{6}$", hcl),
    (grepl("cm", hgt) & between(parse_number(hgt), 150, 193) | 
       grepl("in", hgt) & between(parse_number(hgt), 59, 76)) 
  ) %>%
  nrow()
