x = rmultinom(1e3, 24, prob= c(0.1,0.2,0.3,0.4))

estimates = matrix(NA, nrow=1000, ncol=4)
for (i in 1:1000){
  L = function(p){
    try(-sum(dmultinom(x[,i], size=24, prob=c(p[1],p[2],p[3],p[4]), log=TRUE)), silent = TRUE)
  }
  
  estimates[i,] = optim(c(0.25,0.25,0.3,0.2), L)$par
}

for (i in 1:4){
  for (k in 1:1e3){
    if (estimates[k,i] > 1){
      estimates[k,i] = 0
    }
  }
}

ans = matrix(NA, nrow=1, ncol = 4)
for (i in 1:4){
  ans [, i] = sum(estimates[,i])/1000
}
ans