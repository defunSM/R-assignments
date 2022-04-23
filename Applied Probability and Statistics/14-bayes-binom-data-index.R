# assignment 14
# trying to find where x0, and y0 is located by
# tossing a bunch of points and saying if its
# left/right up/down

x0 = .3
y0 = .8
rel.pos = function(x,y){
  if(x < x0 & y < y0){
    result = c(0,0) }
  if(x > x0 & y < y0){
    result = c(1,0) }
  if(x < x0 & y > y0){
    result = c(0,1) }
  if(x > x0 & y > y0){
    result = c(1,1) }
  result
}

tosses = matrix(runif(200),nrow=100,ncol=2)

data = matrix(NA,nrow=100,ncol=2)
for(i in 1:100){
  data[i,] = rel.pos( tosses[i,1],tosses[i,2] )
}

# X and Y are vectors of 101
X = matrix(NA, nrow=1, ncol=101)
Y = matrix(NA, nrow=1, ncol=101)

p = seq.int(from=0.0, to=1.0, length.out=101)
X = p
Y = p

# 101 x 101 matrix
posterior = matrix(NA, nrow=101, ncol=101)


# confused on how to index the data in the dbinom
for(i in 2:100){
  for(j in 2:100){
    posterior[i,j] = dbinom(data[,1][i],size=98,prob=1-X[i])+dbinom(data[,2][j],size=98,prob=1-Y[j])
    print(posterior[i,j])
  }
}

posterior[1,] = 0
posterior[101,] = 0
posterior[,1] = 0
posterior[,101] = 0

#posterior = scale(posterior, 2, 3, na.rm=TRUE)

#posterior = exp(posterior)

posterior = posterior/((0.01)*sum(posterior)) 








persp(X,Y,posterior, theta=30, ticktype="detailed", nticks=5)