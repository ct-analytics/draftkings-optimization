library(rvest)
library(dplyr)
library(stringr)

# dk.raw <- html("http://rotoguru1.com/cgi-bin/byday.pl?game=dd")
dk.20150521 <- html("http://rotoguru1.com/cgi-bin/byday.pl?date=521&game=dd")

dk.pitchers <- dk.20150521 %>%
  html_nodes("table") %>%
  .[[5]] %>%
   html_table(fill=TRUE) %>%
  mutate(Player = str_replace(X2,"\\^\\d",""),
         Salary = as.numeric(str_replace_all(X4,"\\$|,",""))) %>%
  select(Player,Salary) %>%
  filter(!(is.na(Salary)))

dk.batters <- dk.raw %>%
  html_nodes("table") %>%
  .[[5]] %>%
  html_table(fill=TRUE) %>%
  mutate(Player = str_replace(X2,"\\^\\d",""),
         Salary = as.numeric(str_replace_all(X4,"\\$|,",""))) %>%
  select(Player,Salary) %>%
  filter(row_number() > 126)



