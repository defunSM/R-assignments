# Sept 14
# bisection algo 
# inside the interval [a,b] by the bisection algo and
# using n iteration known as steps

#The default parameters is the set the interval [a, b = [0,1]]
locate.root = function(f,a=0,b=1,n=20) {
  root = (a+b)/2 # choose the midpoint as your initial root approximation
  for(i in 1:n){
    if (f(a)*f(root) < 0){
      b = root
      root = (a+b)/2
    } else {
      a = root
      root = (a+b)/2
    }
  }
  root # final output result
}

f = function(x) { x^3-x-1/4}
locate.root(f,a=-3, b=0)

# We want to make this automatic without having to specify the intervals
# and get all the roots

# We write a code for finding all roots of a function within an interval
# [a,b] with N sub-intervals and n iterations in the bisection algorithm

roots = function(f, a=0, b=1, N=20, n=20) {
  
  roots = NULL
  x = rep(NA, N+1)
  for( i in 1:(N+1)) {
    x[i] = a + (b-a)/N*(i-1)  # vector of sub interval points
  }
  for( i in 1:N){
    if( f(x[i])*f(x[i+1]) < 0) {
      # Apply the bisection algorithm to the interval x[i] to x[i+1]
      roots = c(roots, locate.root(f, a=x[i], b=x[i+1], n)) # combining previous roots
    }
  }
  roots # final output
}

x = roots(f,a=-10, b=10)
x