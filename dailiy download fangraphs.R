library(rvest)
library(dplyr)

date <- Sys.Date()

stats <- list(pitching="pit",batting="bat")

for (i in 1:3) {
  url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=pit&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0&page=",i,"_50",sep="")
  raw <- html(url)
  
  fg <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    filter(row_number() > 9) %>%
    mutate(Date = date,
           Name = X2,
           Team = X3,
           W = as.numeric(X4),
           L = as.numeric(X5),
           SV = as.numeric(X6),
           G = as.numeric(X7),
           GS = as.numeric(X8),
           IP = as.numeric(X9),
           K.per.9 = as.numeric(X10),
           BB.per.9 = as.numeric(X11),
           HR.per.9 = as.numeric(X12),
           BABIP = as.numeric(X13),
           LOB.PCT = as.numeric(str_replace(X14,"%",""))/100,
           GB.PCT = as.numeric(str_replace(X15,"%",""))/100,
           HR.per.FB = as.numeric(str_replace(X16,"%",""))/100,
           ERA = as.numeric(X17),
           FIP = as.numeric(X18),
           xFIP = as.numeric(X19),
           WAR = as.numeric(X20)
           ) %>%
    select(Name,Team,Date,W,L,SV,G,GS,IP,K.per.9,BB.per.9,HR.per.9,BABIP,
           LOB.PCT,GB.PCT,HR.per.FB,ERA,FIP,xFIP,WAR)
  
  if (i==1) {
    fg.pitching.dashboard <- fg
  } else {
    fg.pitching.dashboard <- rbind(fg.pitching.dashboard,fg)
  }
}

url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=",stats$pitching,"&lg=all&qual=0&type=3&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=p",format(date,"%Y-%m-%d"),"page=1_50",sep="")
raw <- html(url)

fg.pitching.winprobability <- raw %>%
  html_nodes("table") %>%
  .[[33]] %>%
  html_table(fill=TRUE) %>%
  select(-1) %>%
  mutate(Date = date)

for (i in 1:14) {
  url <- paste("http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0&page=",i,"_50",sep="")
  raw <- html(url)
  
  fg <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    filter(row_number() > 9) %>% 
    mutate(Name = X2,
           Team = X3,
           Date=date,
           G = as.numeric(X4),
           PA = as.numeric(X5),
           HR = as.numeric(X6),
           R = as.numeric(X7),
           RBI = as.numeric(X8),
           SB = as.numeric(X9),
           BB.PCT = as.numeric(str_replace(X10,"%",""))/100,
           K.PCT = as.numeric(str_replace(X11,"%",""))/100,
           ISO = as.numeric(X12),
           BABIP = as.numeric(X13),
           AVG = as.numeric(X14),
           OBP = as.numeric(X15),
           SLG = as.numeric(X16),
           wOBA = as.numeric(X17),
           wRC.PLUS = as.numeric(X18),
           BsR = as.numeric(X19),
           Off = as.numeric(X20),
           Def = as.numeric(X21),
           WAR = as.numeric(X22)
    ) %>%
    select(Name,Team,Date,G,PA,HR,R,RBI,SB,BB.PCT,K.PCT,ISO,BABIP,AVG,OBP,SLG,wOBA,wRC.PLUS,BsR,Off,Def,WAR)
  
  if (i==1) {
    fg.batting.dashboard <- fg
  } else {
    fg.batting.dashboard <- rbind(fg.batting.dashboard,fg)
  }
}

catchers <- "http://www.fangraphs.com/leaders.aspx?pos=c&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
firstbase <- "http://www.fangraphs.com/leaders.aspx?pos=1b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
secondbase <- "http://www.fangraphs.com/leaders.aspx?pos=2b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
shortstop <- "http://www.fangraphs.com/leaders.aspx?pos=ss&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
thirdbase <- "http://www.fangraphs.com/leaders.aspx?pos=3b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
rightfield <- "http://www.fangraphs.com/leaders.aspx?pos=rf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
centerfield <- "http://www.fangraphs.com/leaders.aspx?pos=cf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
leftfield <- "http://www.fangraphs.com/leaders.aspx?pos=lf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
designatedhitter <- "http://www.fangraphs.com/leaders.aspx?pos=dh&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"

positions <- list("C"=catchers,
                  "1B"=firstbase,
                  "2B"=secondbase,
                  "SS"=shortstop,
                  "3B"=thirdbase,
                  "RF"=rightfield,
                  "CF"=centerfield,
                  "LF"=leftfield,
                  "DH"=designatedhitter)

d <- lapply(seq_along(positions),function(x) {
  n <- names(positions)[x]
  raw <- html(unlist(positions[x]))
  fg <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    mutate(Date=date,
           BB.PCT = as.numeric(str_replace(`BB%`,"%",""))/100,
           K.PCT = as.numeric(str_replace(`K%`,"%",""))/100,
           wRC.PLUS = `wRC+`,
           Position=n) %>%
    select(-`#`,-`BB%`,-`K%`)
  return(fg)
})
