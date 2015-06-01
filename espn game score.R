library(rvest)
library(dplyr)

month <- "5"
day <- "21"

date <- as.Date(paste(month,day,"2015",sep="-"),"%m-%d-%Y")

url <- paste("http://sports.espn.go.com/fantasy/baseball/flb/dailynotes?page=mlbnotes",format(date,"%y%m%d"),sep="")

espn <- html(url)

gs.table <- espn %>%
  html_nodes("table") %>%
  html_table(fill=TRUE)

names(gs.table[[1]]) <- gs.table[[1]][2,] 

gs.pitchers <- gs.table[[1]][-nrow(gs.table[[1]]),c("GS","Name")] %>% 
  mutate(GS=as.numeric(GS)) %>%
  filter(!(is.na(GS)))

library(rCharts)
h <- hPlot(GS ~ Name, 
           data = gs.pitchers %>%
             arrange(desc(GS)) %>%
             slice(1:15), 
           type = c("bar"))
h$tooltip(headerFormat= '<b>{point.x}</b><br>',
          pointFormat= 'Game Score: {point.y:.0f}')
h$yAxis(title=list(text="Game Score"),labels=list(format='{value:.0f}'))
h$title(text=paste("Game Score for ",format(date,"%B %d, %Y"),sep=""))
h$subtitle(text=paste('<i>Data from <a href="',url,' target="_blank"">ESPN</a></i>',sep=""),useHTML=TRUE)
h$save()
h