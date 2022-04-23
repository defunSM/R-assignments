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

U = runif(500)
x_0 = 0
y = 1
#X = (2*atan((-x_0 + U)/y)+pi)/(2*pi)
X = -x_0 + y*tan(pi*(U-1/2))
X = X[X <= 2 & X >= -2]
PDF(X, 20, y.min=0, y.max=1)
curve(1/(pi*y*(1+(x-x_0/y)^2)), add=TRUE, col="red")
#curve ((1/((2*sqrt(pi))*sqrt((1/x)-pi)*x^2)), add = TRUE, col = 'red')
