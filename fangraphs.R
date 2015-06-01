library(rvest)
library(dplyr)
library(stringr)

month <- "5"
day <- "21"

date <- as.Date(paste(month,day,"2015",sep="-"),"%m-%d-%Y")

stats <- list(pitching="pit",batting="bat")

url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=",stats$pitching,"&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p",format(date,"%Y-%m-%d"),"page=1_30",sep="")
             "http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p2015-05-21page=1_30"
url <- "http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p2015-05-21&page=2_30"
# catcher
url <- "http://www.fangraphs.com/leaders.aspx?pos=c&stats=bat&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p2015-05-22"
# url <- "http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p2015-05-22"
raw <- html(url)

fangraphs.dashboard <- raw %>%
  html_nodes("table") %>%
  .[[33]] %>%
  html_table(fill=TRUE) %>%
  select(-1)

url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=0&type=3&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p",format(date,"%Y-%m-%d"),"page=1_30",sep="")
raw <- html(url)

fangraphs.winprobability <- raw %>%
  html_nodes("table") %>%
  .[[33]] %>%
  html_table(fill=TRUE) %>%
  select(-1)

library(rCharts)
h <- hPlot(WAR ~ Name, 
           data = fangraphs.dashboard %>%
             arrange(desc(WAR)) %>%
             slice(1:15), 
           type = c("bar"))
h$tooltip(headerFormat= '<b>{point.x}</b><br>',
          pointFormat= 'WAR: {point.y:.1f}')
h$yAxis(title=list(text="WAR"),labels=list(format='{value:.1f}'))
h$title(text=paste("WAR as of ",format(date,"%B %d, %Y"),sep=""))
h$subtitle(text=paste('<i>Data from <a href="',url,' target="_blank"">Fangraphs</a></i>',sep=""),useHTML=TRUE)
h$save()
h
