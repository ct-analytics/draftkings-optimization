library(rvest)
library(dplyr)
library(stringr)

# dk.raw <- html("http://rotoguru1.com/cgi-bin/byday.pl?game=dd")
month <- "5"
day <- "21"

date.rotoguru <- paste(month,day,sep="")
date <- as.Date(paste(month,day,"2015",sep="-"),"%m-%d-%Y")
url <- paste("http://rotoguru1.com/cgi-bin/byday.pl?date=",date.rotoguru,"&game=dd",sep="")
rotoguru <- html(url)

dk.pitchers <- rotoguru %>%
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
h$title(text=paste("Draft Kings Salaries for ",format(date,"%B %d, %Y"),sep=""))
h$subtitle(text=paste('<i>Data from <a href="',url,' target="_blank"">RotoGuru</a></i>',sep=""),useHTML=TRUE)
h$html()
h
