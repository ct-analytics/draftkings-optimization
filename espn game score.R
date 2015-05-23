library(rvest)

gs.raw <- html("http://sports.espn.go.com/fantasy/baseball/flb/dailynotes?page=mlbnotes150521")

gs.table <- gs.raw %>%
  html_nodes("table") %>%
  html_table(fill=TRUE)

View(gs.table[[2]])

names(gs.table[[1]]) <- gs.table[[1]][2,] 

gs.pitchers <- gs.table[[1]][-nrow(gs.table[[1]]),c("GS","Name")] %>% 
  filter(row_number()>2)

View(gs.pitchers)
