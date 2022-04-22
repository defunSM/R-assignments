# Assignment 10 366

# Linear optimization finding function

# LECTURE OCT 19 366
# line of best fit

x = runif(100, min=-20, max=20)
j = runif(100, min=-5, max=5)
k = runif(100, min=-5, max=5)
y = x+j
z = x+k
plot(x,y)

# What do we mean by best?
# Calculates the line of best fit by minimizing the total errors
# It is assumed x,y are vectors of data of the same length
best.line = function(x,y){
  n = length(y)
  I = diag(n)
  ONE = rep(1, n)
  A.upper = cbind(x, ONE, -I)
  A.lower = cbind(x, ONE, I)
  A = rbind(A.upper, A.lower) # constructing the matrix of constraints
  dir = c( rep("<=", n), rep(">=", n))
  RHS = c(y,y)
  f = c(0,0, rep(1,n)) # Function we are minimizing
  lp("min", f, A, dir, RHS)$solution # the first two are the main interest
}

best.line(x,y) # the first two variables (a and b)

# add the line of best fit
curve(0.98456705*x, add=TRUE, col="red")

# PROBLEM 1
# Minimizing the linear function
#
minimize_func = function(x, y){
  n = length(y)
  I = diag(n)
  ONE = rep(1, n)
  ZERO = rep(0, n)
  A.upper = cbind(x, ONE, rep(-1,n), -I)
  A.lower = cbind(x, ONE, rep(-1,n), I)
  #A.theta2 = cbind(x, ONE, rep(-1, n), I)
  A = rbind(A.upper, A.lower, A.theta) # constructing the matrix of constraints
  dir = c( rep("<=", n), rep(">=", n), rep("<=", n))
  RHS = c(y,y,y)
  
  g = c(0,0,0, rep(1, n)) # a,b, theta the linear function being minimized
  lp("min", g, A, dir, RHS)$solution
}

sol = minimize_func(x,y)
max_error = max(sol[4:length(sol)])
sol
