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

write.lp(ip,"pitcher_optimization.txt",type="lp")

dimnames(ip) <- list(c("Number.Players","Salary"),
                     pitchers$Player)

solve(ip)
get.objective(ip)
get.variables(ip)

get.constraints(ip)

pitchers$Player[as.logical(get.variables(ip))]
pitchers$select <- as.factor(get.variables(ip))

write.csv(pitchers,"pitchers.csv")

library(ggplot2)
library(scales)
library(RColorBrewer)

ggplot(data=pitchers,aes(x=GS,y=Salary,group=select),alpha=abs(select-.2)) +
#   geom_point() +
  geom_text(aes(label=Player,color=select),size=3) +
  scale_y_continuous(labels = dollar) +
  scale_color_discrete(guide=F)

library(rCharts)

pitchers$colors[pitchers$select==1] <- "#66CC99"
pitchers$colors[pitchers$select==0] <- "#CC6666"

h <- hPlot(Salary ~ GS, data = pitchers, 
      type = c("scatter"), group = "Player", size = 3)
# h$tooltip(formatter = "#! function() { return this.name + ', ' + this.x + ', ' + this.y; } !#")
h$tooltip(headerFormat= '<b>{series.name}</b><br>',
          pointFormat= 'Game Score: {point.x}<br>Salary: ${point.y:,.0f}')
# var salary = '$' + point.y.replace(/(\d)(?=(\d\d\d)+(?!\d))/g, "$1,");
h$chart(zoomType = "xy")
h$plotOptions(scatter = list(marker = list(symbol = 'circle')))
h$legend(align = 'right', verticalAlign = 'bottom', layout = 'vertical'
          ,x= -30,y= -60,floating= TRUE,enabled=FALSE)
# h$colors(pitchers$colors)
h$yAxis(title=list(text="Salary"),labels=list(format='${value:,.0f}'))
h$xAxis(title=list(text="Game Score"))
h$title(text="Draft Kings Optimization")
h
  
library(plyr)
l <- dlply(select(pitchers,x=GS,y=Salary,color=colors),1,c)
a <- rCharts::Highcharts$new()
for(i in l) {
  a$series(data = i, type = "scatter", name = pitchers$Player[match(i,l)])
}
a$xAxis(title = list(text = "Game Score"))
a$yAxis(title = list(text = "Salary"))
# a$legend(enabled = F)
a
