setwd("/Users/christopherteixeira/Documents/draftkings optimization/Data")

f <- list.files()

library(stringr)

b <- f[str_detect(f,"batters")]
p <- f[str_detect(f,"pitchers")]

b.df <- lapply(b,function(x) {
  fg <- read.csv(x)
})

p.df <- lapply(p,function(x) {
  fg <- read.csv(x)
})

library(data.table)
batters <- rbindlist(b.df)
pitchers <- rbindlist(p.df)
