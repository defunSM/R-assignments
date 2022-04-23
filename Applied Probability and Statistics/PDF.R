# PROBLEM 2 
# create two uniform random variables

x_1 = runif(10000,min=0,max=1)
x_2 = runif(10000, min=0, max=1)

sum(x_1*x_2 < 1/2)/1000


# Write a code for approximating the PDF from a sample X of some distribution
# where n is the number of sub-intervals you will use
# The default window on the vertical axis is from 0 to 1
PDF = function(X,n,y.min = 0,y.max = 1){
  a = min(X)
  b = max(X)
  N = length(X)
  s = (b-a)/n # spacing of the sub-intervals
  x = rep(NA,n+1)
  x.mid = rep(NA,n)
  for(i in 1:(n+1)){
    x[i] = a + (i-1)*s
  }
  for(i in 1:n){
    x.mid[i] = (.5)*( x[i+1] + x[i] )
  }
  f = rep(NA,n)
  for(i in 1:n){
    f[i] = sum( X > x[i] & X < x[i+1] )/(N*s)
  }
  plot(x.mid,f,type = "l",ylim = c(y.min,y.max) )
}

set.seed(923)
PDF(x_1*x_2, 20, y.max=5)