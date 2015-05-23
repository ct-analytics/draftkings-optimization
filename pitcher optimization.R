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
