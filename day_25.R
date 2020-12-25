data25 <- read.table("Input/day25.txt")[,1]

pub_key <- 1
enc_key <- 1
while (data25[1] != pub_key) {
  pub_key <- (7 * pub_key) %% 20201227
  enc_key <- (data25[2] * enc_key) %% 20201227
}

enc_key
