# Assignment 11
# Euler's method vs Runge Kutta method

# Function we are testing
exact.ans = function(x) { exp(0.5*x^2) } # exact from wolframalpha
f = function(t,y){ t*y}
euler.method.graph(f,0,1,1, 20)
curve(exact.ans, add=TRUE, col="red")

runge.kutta = function(f, t0=0, t.f=1, y0=1, n=20){
  y = matrix(NA, nrow=1, ncol=n+1)
  t = matrix(NA, nrow=1, ncol=n+1)
  y[1] = y0
  t[1] = t0
  eps = (t.f - t0)/n
  for( i in 2:(n+1)){
    c1 = f(t0, y0)
    c2 = f(t0+(1/2)*eps, y0+(1/2)*eps*c1)
    c3 = f(t0 + (1/2)*eps, y0+(1/2)*eps*c2)
    c4 = f(t0+eps, y0+eps*c3)
    y[i] = y[i-1] + (1/6)*eps*(c1+2*c2+2*c3+c4)
    t0 = t0 + eps
    t[i] = t0
    y0 = y[i]
  }
  lines(t,y, col="green")
  y
}
x = runge.kutta(f,t.f=1,y0=1,n=20)

