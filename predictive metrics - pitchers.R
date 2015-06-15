setwd("/Users/christopherteixeira/Documents/draftkings optimization/")
source("salary data till now.R")
source("compile fangraphs data.R")

library(dplyr)

df <- inner_join(draftkings,
                 pitchers,
                 by=c("Player"="Name","Date"="Date")) 

numeric.vars <- names(df)[sapply(df,is.numeric)]
character.vars <- names(df)[!sapply(df,is.numeric)]

library(ggplot2)
g <- ggplot(data=df, aes(y=Points)) 

g + geom_point(aes(x=WAR))
g + geom_point(aes(x=xFIP))
g + geom_point(aes(x=FIP))

correlations <- as.data.frame(t(cor(df$Points,select(df,-Team,-Player,-Points,-Date,-Position))))
names(correlations) <- c("Correlation")
arrange(correlations,Correlation)
correlations$Statistic <- rownames(correlations)

library(DT)
datatable(correlations %>% select(Correlation))

library(rCharts)
h <- rPlot(Points ~ WAR,
      data=df,
      color="Position",
      type = "point")
h$addControls("x",value="WAR",values=numeric.vars)
h$addControls("color", value = "Position", values = character.vars)
h

h <- dPlot(Points ~ WAR,
           groups = 'Position',
           data=df,
           type = "bubble")
h$legend(x = 200,y = 10,width = 500,height = 20,horizontalAlign = "right")
h
