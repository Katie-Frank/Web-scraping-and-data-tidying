library(rvest)
library(tidyr)
library(dplyr)

rio_dec <- read_html("https://en.wikipedia.org/wiki/Athletics_at_the_2016_Summer_Olympics_%E2%80%93_Men%27s_decathlon")

tbls <- rio_dec %>% 
  html_nodes("table")

dec <- tbls[[19]] %>% 
  html_table()

names(dec) <- c("rank", "athlete", "nationality",
  "overall_points", "100m", "LJ", "SP", "HJ", "400m",
  "110mH", "DT", "PV", "JT", "1500m")

dec$rank[1:3] <- 1:3
dec$rank <- na_if(dec$rank, "n/a") %>% 
  as.numeric()

dec <- separate(dec, overall_points, c("overall_points", "achievement"), sep = 4)
dec$achievement <- recode(dec$achievement, "=OR" = "OR")
dec$achievement <- na_if(dec$achievement, "")

# separating event columns
dec <- dec %>% 
  separate("100m", c("100m_points", "100m"), sep = "(?=\\d{2}[.])") %>% 
  separate(LJ, c("LJ_points", "LJ"), sep = "(?=\\d{1}[.])", 
           convert = TRUE) %>% 
  separate(SP, c("SP_points", "SP"), sep = "(?=\\d{2}[.])|(?=NM)") %>% 
  separate(HJ, c("HJ_points", "HJ"), sep = 3, convert = TRUE) %>% 
  separate(`400m`, c("400m_points", "400m"), sep = "(?=\\d{2}[.])",
           convert = TRUE) %>% 
  separate(`110mH`, c("110mH_points", "110mH"), sep = "(?=\\d{2}[.])",
                  convert = TRUE) %>% 
  separate(DT, c("DT_points", "DT"), sep = "(?=\\d{2}[.])|(?=NM)") %>% 
  separate(PV, c("PV_points", "PV"), sep = "(?=\\d{1}[.])|(?=NM)") %>% 
  separate(JT, c("JT_points", "JT"), sep = "(?=\\d{2}[.])",
           convert = TRUE) %>% 
  separate(`1500m`, c("1500m_points", "1500m"), 
           sep = "(?=\\d{1}\\:)|(?=DNF)")

# reording columns
dec <- dec[, c(1:5, as.vector(rbind(seq(7, 25, 2), seq(6, 24, 2))))]

head(dec)