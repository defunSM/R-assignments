# assignment 12
# Second order differential equations solving with SODE
# Uses clever trick of x = y' to convert problem into
# first order DE

euler.method.system = function(f,g, t0, x0, y0, t.f, n) {
  eps = (t.f - t0)/n
  t = rep(NA, n+1)
  x = rep(NA, n+1) # adding this
  y = rep(NA, n+1)
  t[1] = t0
  y[1] = y0
  x[1] = x0 # adding this
  for (i in 2:(n+1)){
    x[i] = x[i-1] + eps*f(t[i-1], x[i-1], y[i-1])
    y[i] = y[i-1] + eps*g(t[i-1], x[i-1], y[i-1])
    t[i] = t[i-1] + eps
  }
  plot(t,x,type="l") # b for a different type or l for line
  plot(t,y,type="l")
  #plot(x,y,type="l")
  y
}

SODE = function(a,b,c, t0=0, y0=1, y0.=1, t.f=1, n=20){
  f = function(t,x,y){ c(t)-(a(t)*x)-(b(t)*y) }
  g = function(t,x,y){ x }
  
  euler.method.system(f,g, t0, y0., y0, t.f, n)
  
}

a = function(t){ -4*t }
b = function(t){ 4*(t^2) - 2 }
c = function(t){ 0 }

SODE(a,b,c, y0=1, y0.=0, t.f=3, n=30)
exact.ans = function(t){ exp(t^2)}
curve(exact.ans, add=TRUE, col="red")
exact.ans(3)

f = function(t,x,y){y}
g = function(t,x,y){-x}
euler.method.system(f,g,0,0,1,2*pi, 5000)
