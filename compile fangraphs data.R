setwd("/Users/christopherteixeira/Documents/draftkings optimization/Data")

f <- list.files()

library(stringr)

b <- f[str_detect(f,"batters")]
p <- f[str_detect(f,"pitchers")]

b.df <- lapply(b,function(x) {
  fg <- read.csv(x)
  return(fg)
})

p.df <- lapply(p,function(x) {
  fg <- read.csv(x)
  return(fg)
})

library(data.table)
batters <- rbindlist(b.df)
pitchers <- rbindlist(p.df)

pitchers <- pitchers %>%
  mutate(Date=as.Date(Date)) %>%
  select(-X) %>%
  as.data.frame()

batters <- batters %>%
  mutate(Date=as.Date(Date)) %>%
  select(-X) %>%
  as.data.frame()

rm(b.df,p.df,b,p,f)
