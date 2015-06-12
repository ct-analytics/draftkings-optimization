suppressWarnings(library(rvest,warn.conflicts=F,quietly=T))
suppressWarnings(library(dplyr,warn.conflicts=F,quietly=T))
suppressWarnings(library(stringr,warn.conflicts=F,quietly=T))
suppressWarnings(library(data.table,warn.conflicts=F,quietly=T))
suppressWarnings(library(methods,warn.conflicts=F,quietly=T))

date <- Sys.Date()

cat("Getting data for",format(date,"%B %d, %Y"),"...\n")

starters <- "http://www.fangraphs.com/leaders.aspx?pos=all&stats=sta&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_50"
relievers <- "http://www.fangraphs.com/leaders.aspx?pos=all&stats=rel&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0&page=1_50"

pitchers <- list("SP"=starters,
                 "RP"=relievers)

p <- lapply(seq_along(pitchers),function(x) {
  n <- names(pitchers)[x]
  raw <- html(unlist(pitchers[x]))
  fg <- raw %>%
    html_nodes("table") %>%
    .[[33]] %>%
    html_table(fill=TRUE) %>%
    filter(row_number() > 9) %>%
    mutate(Date=date,
           Name=X2,
           Team=X3,
           W=as.numeric(X4),
           L=as.numeric(X5),
           SV=as.numeric(X6),
           G=as.numeric(X7),
           GS=as.numeric(X8),
           IP=as.numeric(X9),
           K.per.9=as.numeric(X10),
           BB.per.9=as.numeric(X11),
           HR.per.9=as.numeric(X12),
           BABIP=as.numeric(X13),
           LOB.pct=as.numeric(str_replace(X14,'%','')),
           GB.pct=as.numeric(str_replace(X15,'%','')),
           HR.per.FB=as.numeric(str_replace(X16,'%','')),
           ERA=as.numeric(X17),
           FIP=as.numeric(X18),
           xFIP=as.numeric(X19),
           WAR=as.numeric(X20),
           Position=n) %>%
    select(Name,Team,Date,Position,W,L,SV,G,GS,IP,K.per.9,
           BB.per.9,HR.per.9,BABIP,LOB.pct,GB.pct,HR.per.FB,ERA,FIP,xFIP,WAR)
  return(fg)
})

cat("Finished pitchers...\n")

catchers <- "http://www.fangraphs.com/leaders.aspx?pos=c&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
firstbase <- "http://www.fangraphs.com/leaders.aspx?pos=1b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
secondbase <- "http://www.fangraphs.com/leaders.aspx?pos=2b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
shortstop <- "http://www.fangraphs.com/leaders.aspx?pos=ss&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
thirdbase <- "http://www.fangraphs.com/leaders.aspx?pos=3b&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
rightfield <- "http://www.fangraphs.com/leaders.aspx?pos=rf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
centerfield <- "http://www.fangraphs.com/leaders.aspx?pos=cf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
leftfield <- "http://www.fangraphs.com/leaders.aspx?pos=lf&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"
designatedhitter <- "http://www.fangraphs.com/leaders.aspx?pos=dh&stats=bat&lg=all&qual=y&type=8&season=2015&month=0&season1=2015&ind=0&team=0&rost=0&age=0&filter=&players=0"

batters <- list("C"=catchers,
                  "1B"=firstbase,
                  "2B"=secondbase,
                  "SS"=shortstop,
                  "3B"=thirdbase,
                  "RF"=rightfield,
                  "CF"=centerfield,
                  "LF"=leftfield,
                  "DH"=designatedhitter)

b <- lapply(seq_along(batters),function(x) {
  n <- names(batters)[x]
  raw <- html(unlist(batters[x]))
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
cat("Finished batters...\n")

b.full <- rbindlist(b)
p.full <- rbindlist(p)

cat("Writing out data...\n")

write.csv(b.full,paste("/Users/christopherteixeira/Documents/draftkings\ optimization/Data/fangraphs-batters-",format(date,"%Y%m%d"),".csv",sep=""))
write.csv(p.full,paste("/Users/christopherteixeira/Documents/draftkings\ optimization/Data/fangraphs-pitchers-",format(date,"%Y%m%d"),".csv",sep=""))

cat("Finished gathering data\n")