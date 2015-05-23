library(dplyr)

pitchers <- inner_join(dk.pitchers,gs.pitchers,by=c("Player"="Name")) %>%
  mutate(GS = as.numeric(GS),
         value = Salary / GS) %>%
  arrange(value)

