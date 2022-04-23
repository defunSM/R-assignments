# Assignment 15

# Problem 1 Moving particles around in a bowel
x0 = 0 # number in the left bowel starting off
N = 3 # Total number of particles
P = matrix(0, nrow=N+1, ncol=N+1)

# This one creates the transition matrix for a given N
for(i in 1:(N+1)){
  for(j in 1:(N+1)){
    if(j-i == 1){
      P[i,j] = 1-((i-1)/N)
    } 
    else if ( i-j == 1){
      P[i,j] = (i-1)/N
    } else {
      P[i,j] = 0
    }
  }
}
P

# Problem 2
# Flipping a coin symmetric random walk with a reflecting boundary
set.seed(401)
x0 = 1 # this is between 1<x0<N
N = 10000000
steps = 999
X = matrix(NA, nrow=1, ncol=steps) # markov chain
X[1] = x0
for (i in 2:steps){
  decision = runif(1)
  if ( decision > 1/2 & X[i-1] != N){ # heads
    X[i] = X[i-1] + 1
  } else if ( decision < 1/2 & X[i-1] != 1) { # tails
    X[i] = X[i-1] - 1
  } else if ( X[i-1] == N){
    X[i] = X[i-1] - 1
  } else if ( X[i-1] == 1){
    X[i] = X[i-1] + 1
  }
}
mean(X)
table(X)/steps
hist(X)

# IGNORE THE BELOW incomplete code
# This code generates the first row for the transition matrix
setup = function(x0, N){
  P = matrix(NA, nrow=N+1, ncol=N+1) # transition matrix of how many are in the left bowel
  P[1,] = 0
  for(j in 1:(N+1)){
    if(x0 == 0){
      P[1,2] = 1  
    }
    if(x0 != 0 & x0 != N ){
      P[1,x0] = x0/N
      P[1,x0+2] = 1-(x0/N)
      P[1,x0+1] = 0
    }
    
    if(x0 == N){
      P[1,N] = 1
    }
  }
  P
}
# Using the initial first row transition matrix
create.transition.matrix = function(P){
  for(i in 2:(N+1)){
    for(j in 1:(N+1)){
      if(j!=1){
        #print(P[i-1,j])
        if( P[i-1,j] != 0){
          P[i,] = 0
          P[i,j] = 0
          if(j!=N+1){
            P[i,j-1] = (i-1)/N
            P[i,j+1] = 1-((i-1)/N)
          } else {
            #P[i,j+1] = 1-((i-1)/N)
            P[i,j-1] = (i-1)/N
          }
        }
      } else {
        P[i,j] = 0
      }
      
    }
    print(P)
  }
  P
}

P = setup(x0, N)
P = create.transition.matrix(P)


for(i in 1:(N+1)){
  for(j in 1:(N+1)){
    P[i,j] = 1
  }
}

# Problem 1 Moving particles around in a bowel
x0 = 2 # number in the left bowel starting off
N = 5 # Total number of particles

p1 = x0/N
p2 = 1-(x0/N)
P = matrix(0, nrow=N+1, ncol=N+1)
for(i in 1:(N+1)){
  if(x0==0 & i==1){
    P[1,2] = 1
  }
  if(x0==N & i == 1){
    P[1,N] = 1
  }
  if((x0 != N & x0 !=0) & i == 1){
    P[1,x0] = p1
    P[1, x0+2] = p2
  }
  if(i!=1){
    if (x0-i > 1){
      P[i,x0-i] = p[i-1, x0]/2  
    }
  }
}
P


#P[1,x0] = (1-x0/N)  # This is a fix for probability row adding to 1
# since the code is not setting x0 in the matrix we do this manually. 
# remove this to figure out why this is necessary and will notice the first row
# of probabilities dont add to 1

#P[N+1,N] = P[N+1,N] - x0/N
