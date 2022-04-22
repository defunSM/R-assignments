laplace.matrix = function(n){
  M = rep(0,n^4)
  M = matrix(M,nrow = n^2,ncol = n^2)
  
  M[1,1] = 4
  M[1,2] = -1
  M[1,n+1] = -1
  M[n,n] = 4
  M[n,n-1] = -1
  M[n,2*n] = -1
  M[n^2-n+1,n^2-n+1] = 4
  M[n^2-n+1,n^2-2*n+1] = -1
  M[n^2-n+1,n^2-n+2] = -1
  M[n^2,n^2] = 4
  M[n^2,n^2-1] = -1
  M[n^2,n^2-n] = -1
  
  for(i in 2:(n-1))
  {
    M[i,i] = 4
    M[i,i-1] = -1
    M[i,i+1] = -1
    M[i,i+n] = -1
  }
    
  for(i in 1:(n-2))
  {
    M[i*n+1,i*n+1] = 4
    M[i*n+1,i*n+2] = -1
    M[i*n+1,i*n+1+n] = -1
    M[i*n+1,i*n+1-n] = -1
  }

  for(i in 2:(n-1))
  {
    M[i*n,i*n] = 4
    M[i*n,i*n-1] = -1
    M[i*n,(i-1)*n] = - 1
    M[i*n,(i+1)*n] = -1
  }
  
  for(i in 2:(n-1))
  {
    M[n^2-n+i,n^2-n+i] = 4
    M[n^2-n+i,n^2-n+i+1] = -1
    M[n^2-n+i,n^2-n+i-1] = -1
    M[n^2-n+i,n^2-2*n+i] = -1
  }
  
  for(i in 2:(n-1))
    {
    for(j in 1:(n-2))
      {
        M[j*n+i,j*n+i] = 4
        M[j*n+i,j*n+i-1] = -1
        M[j*n+i,j*n+i+1] = -1
        M[j*n+i,j*n+i+n] = -1
        M[j*n+i,j*n+i-n] = -1
    }
  }
  
  M
}

boundary.vector = function(lower,right,upper,left,n){
  dx = 1/(n+1)
  dy = dx
  b0 = rep(0,n^2)
  
  b.lower = b0
  for(i in 1:n){
  b.lower[i] = lower(i*dx) 
  }
  
  b.right = b0
  for(i in 1:n){
    b.right[i*n] = right(i*dy) 
  }
  
  b.upper = b0
  
  #n=3
  #dx=0.1
  for(i in 1:n){
    b.upper[n^2-(i-1)] = upper(i*dx)
  }
  print(b.upper)
  b.left = b0
  b.left[1] = left(dy)
  for(i in 1:n){
    b.left[1+(n*i)] = left((i+1)*dy)
  }
  b.left[n^2+1] = 0
  print(b.left)
  b = b.lower + b.right + b.upper + b.left
  b
  
  # this code is incomplete you need a loop for b.upper and b.left
  # the output vector b will then be equal to b.lower + b.right + b.upper + b.left
  
}

# solving
f.lower = function(s){
  sin(pi*s)
}
zero.func = function(s){
  0
}

A = laplace.matrix(4)
b = boundary.vector(f.lower, zero.func, zero.func, zero.func, 4)

u = rref( cbind(A, b))

# Creating laplace.solve
laplace.solve = function(lower,right,upper,left,n){
  A = laplace.matrix(n)
  b = boundary.vector(lower,right,upper,left,n)
  U = rref(cbind(A, b))[,n*n+1]
  U = matrix(U, nrow=n, ncol=n, byrow=TRUE)
  U
}

laplace.solve(f.lower, zero.func, zero.func, zero.func, 4) # this n is the outside grid number
# Working out the example in class
f.lower = function(s){ sin(pi*s)*cos(pi*s)}
f.upper = function(s){ sin(2*pi*s)}
f.left = function(s){ s*(1-s)}
f.right = function(s){ (s*(1-s))^2}

A = laplace.solve(f.lower, f.right, f.upper, f.left, 9)