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


library(rCharts)
h <- hPlot(Salary ~ Player, 
           data = dk.pitchers %>%
             arrange(desc(Salary)) %>%
             slice(1:15), 
           type = c("bar"))
h$tooltip(headerFormat= '<b>{point.x}</b><br>',
          pointFormat= 'Salary: ${point.y:,.0f}')
h$yAxis(title=list(text="Salary"),labels=list(format='${value:,.0f}'))

outf <- h$print()
str(h)
    output <- file("salary chart.txt")
writeLines(h$print(),output)
close(output)
