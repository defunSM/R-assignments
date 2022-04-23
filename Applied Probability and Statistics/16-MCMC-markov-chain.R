# Assignment 16
# MCMC Sampling using markov chain
# lecture Dec 2

# Constructing a markov chain
set.seed(401)
# PROBLEM 1 Part 1
#not normalized
f = function(k){
  if(k>0){
    1/k^2    
  } else {
    0
  }
}

x0=1
N = 1e4

x = rep(0,N)
x[1] = x0
for(n in 1:N){
  
  proposal = x[n] + sample(c(1,-1), size=1) # proposals are symmetric    
  
  if( f(proposal) > f(x[n])){
    x[n+1] = proposal
  } else {
    x[n+1] = x[n]
    r = f(proposal)/f(x[n]) # this is a number < 1
    if( runif(1) < r){
      x[n+1] = proposal
    }
  }
}
# part 1 markov chain
x
hist(x)
table(x)/N
mean(x)
# PROBLEM 1 Part 2

# Problem 2 MCMC

f = function(s,t){
  if(s^2+t^2 < 1){
    sqrt(1-s^2-t^2)  
  } else {
    c(0,0)
  }
}


x = matrix(NA,nrow=1e3,ncol=2)
x[1,] = c(0,0)


#s.eps = 0.01
#t.eps = 0.01
N = 1e3

for(n in 1:N){
  
  
  proposal.s = x[n,1] + rnorm(1)
  proposal.t = x[n,2] + rnorm(1)
  proposal = c(proposal.s, proposal.t) # proposals are symmetric    
  
  if( f(proposal[1], proposal[2]) > f(x[n,1], x[n,2])){
    x[n+1,] = proposal
  } else {
    x[n+1,] = c(x[n,1], x[n,2])
    r = f(proposal[1], proposal[2])/f(x[n,1], x[n,2]) # this is a number < 1
    if( runif(1) < r){
      x[n+1,] = proposal
    }
  }
}

plot(x)
mean(x)
var(x)

