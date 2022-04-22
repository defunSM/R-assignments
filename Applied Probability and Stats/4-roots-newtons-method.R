# Assignment 4 - MATH 366
#PROBLEM 1
#
# Newtons Method

x_0 = 0 # any number

# How do you take the derivative of a function in r?
f = function(x){
  2*x-3
}

g = function(x){
  2
}

# calculates the root of a function
# f is the function that we r trying to find the root of
# g is the derivative of f
# x_0 is the iniital guess
# n is the number of iterations
newtons.method = function(f,g, x_0, n) {
  
  A = matrix(NA, nrow=1, ncol=n)
  A[1] = x_0
  for (i in 2:n){
    A[i] = A[i-1] - f(A[i-1])/g(A[i-1])
  }
  A[n]
}

newtons.method(f,g,0,1e4)

# Problem 2
# A matrix norm is a way of assigning a numerical measurement

mat.norm = function(A, type=c("one", "inf", "F")){
  rows = nrow(A)
  cols = ncol(A)
  
  if(type=="F"){
    norm = matrix(NA, nrow=rows, ncol=cols)
    for (i in 1:rows){
      for ( j in 1:cols) {
        norm[i,j] = A[i,j]^2
      }
    }
    sqrt(sum(norm))
  } 
  else if (type == "one"){
    norm = matrix(NA, nrow=1, ncol=rows)
    for( i in 1:rows){
      norm[i] = sum(abs(A[,i]))
    }
    print(norm)
    max(norm)
  } else {
    norm = matrix(NA, nrow=1, ncol=cols)
    for(i in 1:cols){
      norm[i] = sum(abs(A[i,]))
    }
    print(norm)
    max(norm)
  }
}
A = matrix(c(2,1,3,4), nrow=2, ncol=2)
A
mat.norm(A, type="one")
