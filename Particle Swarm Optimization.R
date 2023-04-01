library(pso)

sphere = function(x){
  sum(x^2)
}

D = 3
maxit = 10
s = 3

C = list(trace = 1, maxit = maxit, REPORT=1, trace.stats=1, s=s)

PSO = psoptim(rep(NA, D), fn =sphere, lower=rep(-5, D), upper = rep(5, D), control = C)

j = 1
plot(xlim=c(1, maxit), rep(1, s), PSO$stats$x[[1]][j,], pch = 16,
     xlab = "iterations", ylab = paste("s_", j, " value", sep=""))
for(i in 2:maxit)
  points(rep(i, s), PSO$stats$x[[i]][j,], pch=16)

j = 2
plot(xlim=c(1, maxit), rep(1, s), PSO$stats$x[[1]][j,], pch = 16,
     xlab = "iterations", ylab = paste("s_", j, " value", sep=""))
for(i in 2:maxit)
  points(rep(i, s), PSO$stats$x[[i]][j,], pch=16)

j = 3
plot(xlim=c(1, maxit), rep(1, s), PSO$stats$x[[1]][j,], pch = 16,
     xlab = "iterations", ylab = paste("s_", j, " value", sep=""))
for(i in 2:maxit)
  points(rep(i, s), PSO$stats$x[[i]][j,], pch=16)


plot(PSO$stats$error, type = "l", lwd=2, xlab="iterations", ylab="best fitness")

cat("best: ", PSO$par, "f: ", PSO$value, "\n")

