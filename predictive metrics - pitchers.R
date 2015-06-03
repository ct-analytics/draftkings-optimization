library(dplyr)
library(stringr)

pitchers <- inner_join(dk.pitchers,gs.pitchers,by=c("Player"="Name")) %>%
  inner_join(fangraphs.dashboard,by=c("Player"="Name")) %>%
  inner_join(fangraphs.winprobability,by=c("Player"="Name")) %>%
  rename(Game.Score=GS.x,
         Team=Team.x,
         GS=GS.y,
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
