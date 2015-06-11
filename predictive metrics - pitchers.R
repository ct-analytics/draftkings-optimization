library(rvest)
library(dplyr)
library(stringr)

month <- "5"
day <- str_pad(as.character(1:30),2,pad="0")

for (d in day) {
  date <- as.Date(paste(month,d,"2015",sep="-"),"%m-%d-%Y")
  date.rotoguru <- paste(month,d,sep="")
  url <- paste("http://rotoguru1.com/cgi-bin/byday.pl?date=",date.rotoguru,"&game=dd",sep="")
  rotoguru <- html(url)
  
  dk <- rotoguru %>%
    html_nodes("table") %>%
    .[[5]] %>%
    html_table(fill=TRUE) %>%
    mutate(Player = str_replace(X2,"\\^\\d?",""),
           Salary = as.numeric(str_replace_all(X4,"\\$|,","")),
           Points = as.numeric(X3),
           Date = date) %>%
    select(Player,Date,Salary,Points) %>%
    filter(!(is.na(Salary)))
  
  if (d==day[1]) {
    dk.pitchers <- dk
  } else {
    dk.pitchers <- rbind(dk.pitchers,dk)
  }
}

rm(dk,rotoguru,url,date,date.rotoguru)
stats <- list(pitching="pit",batting="bat")

day <- day[!day %in% c("06","23")]

for (d in day) {
  date <- as.Date(paste(month,d,"2015",sep="-"),"%m-%d-%Y")
  
  url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=",stats$pitching,"&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p",format(date,"%Y-%m-%d"),"page=1_50",sep="")
  raw <- html(url)
  
  fg.dashboard <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    select(-1) %>%
    mutate(Date = date)
  
  url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=",stats$pitching,"&lg=all&qual=0&type=3&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p",format(date,"%Y-%m-%d"),"page=1_50",sep="")
  raw <- html(url)
  
  fg.winprobability <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    select(-1) %>%
    mutate(Date = date)
  
  if (d==day[1]) {
    fangraphs.dashboard <- fg.dashboard
    fangraphs.winprobability <- fg.winprobability
  } else {
    fangraphs.dashboard <- rbind(fangraphs.dashboard,fg.dashboard)
    fangraphs.winprobability <- rbind(fangraphs.winprobability,fg.winprobability)
  }
}

rm(d,fg.dashboard,fg.winprobability,date,date.rotoguru,raw,url,stats)

pitchers <- inner_join(dk.pitchers,fangraphs.dashboard,by=c("Player"="Name","Date"="Date")) %>%
  inner_join(fangraphs.winprobability,by=c("Player"="Name","Date"="Date")) %>%
  rename(Team=Team.x,
         K.per.9=`K/9`,
         BB.per.9=`BB/9`,
         HR.per.9=`HR/9`,
         WPA.minus=`-WPA`,
         WPA.plus=`+WPA`,
         WPA.per.LI=`WPA/LI`) %>%
  mutate(LOB.pct=as.numeric(str_replace(`LOB%`,'%','')),
         GB.pct=as.numeric(str_replace(`GB%`,'%','')),
         HR.per.FB=as.numeric(str_replace(`HR/FB`,'%',''))) %>%
  select(-c(Team.y,`LOB%`,`GB%`,`HR/FB`))

library(ggplot2)
ggplot(data=pitchers, aes(y=Points)) +
  geom_point(aes(x=Game.Score))

ggplot(data=pitchers, aes(y=Points)) +
  geom_point(aes(x=WPA))

correlations <- as.data.frame(t(cor(pitchers$Points,select(pitchers,-Team,-Player,-Points))))
names(correlations) <- c("Correlation")
arrange(correlations,Correlation)
rownames(correlations)
