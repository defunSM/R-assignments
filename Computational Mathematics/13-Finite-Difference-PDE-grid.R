# Assignment 13
# November 16 lecture
# Let us create a grid where Delta_x = 0.1
# Then Delta_t = (1/2)*(Delta_x)^2 = 0.005
dt = 0.005
dx = 0.1

# Let us make a grid from x = -5 to x = 5
# and form t=0 to t = 0.5
x = seq(from=0, to=1, by=dx)
t = seq(from=0, to=0.02, by=dt)

A = matrix(NA, nrow=5, ncol=11)

for(i in 1:11){
  A[1,i] = sqrt(((i-1)*dx)*(1-((i-1)*dx)))
}
A[,1] = 0
A[,11] = 0


#A[1,51] = 1/dx

for(i in 2:4){
  for(j in 2:10){
    A[i,j] = 0.525*A[i-1, j-1] + 0.475*A[i-1, j + 1]
  }
}

persp(t,x, A, theta=30)