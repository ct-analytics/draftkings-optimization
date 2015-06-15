library(rvest)
library(dplyr)
library(stringr)
library(data.table)

start.date <- as.Date("05-01-2015","%m-%d-%Y")
today <- Sys.Date()
days.to.retrieve <- seq(start.date,today,"day")

dk.full <- lapply(days.to.retrieve,function(d) {
  d.format <- str_replace(format(d,"%m%d"),"^[0]","")
  url <- paste("http://rotoguru1.com/cgi-bin/byday.pl?date=",d.format,"&game=dd",sep="")
  rotoguru <- html(url)
  
  dk <- rotoguru %>%
    html_nodes("table") %>%
    .[[5]] %>%
    html_table(fill=TRUE) %>%
    mutate(Player = str_replace(X2,"\\^\\d",""),
           Salary = as.numeric(str_replace_all(X4,"\\$|,","")),
           Points = as.numeric(X3),
           Date = d) %>%
    select(Player,Date,Salary,Points) %>%
    filter(!(is.na(Salary)))
  return(dk)
})

draftkings <- as.data.frame(rbindlist(dk.full))

rm(dk.full,days.to.retrieve,start.date,today)