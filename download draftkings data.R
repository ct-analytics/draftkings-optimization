library(rvest)
library(dplyr)

dk.raw <- html("http://rotoguru1.com/cgi-bin/byday.pl?game=dd")

dk.pitchers <- dk.raw %>%
  html_nodes("table") %>%
  .[[5]] %>%
   html_table(fill=TRUE) %>%
  mutate(Player = str_replace(X2,"\\^\\d","")) %>%
  select(Player,Salary=X4) %>%
  filter(row_number() > 5) %>%
  slice(1:119)

dk.batters <- dk.raw %>%
  html_nodes("table") %>%
  .[[5]] %>%
  html_table(fill=TRUE) %>%
  mutate(Player = str_replace(X2,"\\^\\d","")) %>%
  select(Player,Salary=X4) %>%
  filter(row_number() > 126)

library(stringr)
str_replace(dk.table$Player,"\\^\\d","")
