pitchers <- inner_join(dk.pitchers,gs.pitchers,by=c("Player"="Name")) %>%
  mutate(GS = as.numeric(GS),
         value = Salary / GS) %>%
  arrange(value)

library(lpSolve)
library(lpSolveAPI)

num.pitchers.select <- 2
salary.cap <- 25000

ip <- make.lp(0,nrow(pitchers))

# Select only two pitchers
add.constraint(ip,
               rep(1,nrow(pitchers)),
               "<=",
               num.pitchers.select
)

# Salary cap
add.constraint(ip,
               pitchers$Salary,
               "<=",
               salary.cap
)

set.type(ip,seq(1,nrow(pitchers)),type="binary")

set.objfn(ip,pitchers$GS)
lp.control(ip,sense="max")

dimnames(ip) <- list(c("Number.Players","Salary"),
                     pitchers$Player)
write.lp(ip,"pitcher_optimization.txt",type="lp")

solution.status <- solve(ip)
status <- c("0"="optimal solution found",
            "1"="the model is sub-optimal",
            "2"="the model is infeasible",
            "3"="the model is unbounded",
            "4"="the model is degenerate",
            "5"="numerical failure encountered",
            "6"="process aborted",
            "7"="timeout",
            "9"="the model was solved by presolve",
            "10"="the branch and bound routine failed",
            "11"="the branch and bound was stopped because of a break-at-first or break-at-value",
            "12"="a feasible branch and bound solution was found",
            "13"="no feasible branch and bound solution was found")

cat(paste("IP Solved with status: ",status[as.character(solution.status)],"\n",sep=""))

cat(paste("Optimal game score value: ",get.objective(ip),"\n",sep=""))

cat(paste("Salary used: ",paste("$",format(get.constraints(ip)[2],big.mark=","),sep=""),"\n",sep=""))

cat(paste("Pitchers selected: ",paste(pitchers$Player[as.logical(get.variables(ip))],collapse="\n"),sep="\n"))
pitchers$select <- as.factor(get.variables(ip))

write.csv(pitchers,"pitchers.csv")

library(rCharts)

pitchers$colors[pitchers$select==1] <- "#66CC99"
pitchers$colors[pitchers$select==0] <- "#CC6666"

h <- rCharts::Highcharts$new()
selected <- pitchers %>%
  filter(select == 1)
for (i in 1:nrow(selected)) {
  s.tmp <- list(name=selected$Player[i],x=selected$GS[i],y=selected$Salary[i])
  if (i==1)
    s <- list(s.tmp)
  else
    s <- c(s,list(s.tmp))
}
notselected <- pitchers %>%
  filter(select != 1)
for (i in 1:nrow(notselected)) {
  ns.tmp <- list(name=notselected$Player[i],x=notselected$GS[i],y=notselected$Salary[i])
  if (i==1)
    ns <- list(ns.tmp)
  else
    ns <- c(ns,list(ns.tmp))
}
h$series(data = s,name="Selected",type="scatter",marker=list(radius=3))
h$series(data = ns,name="Not Selected",type="scatter",marker=list(radius=3))
h$tooltip(headerFormat="",
  pointFormat='<b>{point.name}</b><br>Game Score: {point.x}<br>Salary: ${point.y:,.0f}')
h$plotOptions(scatter = list(marker = list(symbol = 'circle')))
h$legend(align = 'right', verticalAlign = 'bottom', layout = 'vertical'
         ,x= -30,y= -60,floating= TRUE,enabled=TRUE)
h$yAxis(title=list(text="Salary"),labels=list(format='${value:,.0f}'))
h$xAxis(title=list(text="Game Score"))
h$title(text="Draft Kings Optimization")
h$chart(zoomType = "xy")
h
