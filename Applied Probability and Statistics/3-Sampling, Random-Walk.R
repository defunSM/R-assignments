# Assignment 3 - MATH 377 
# --------------------------------------------------------
#Problem 1:
#  Recall the “Binomial Random Variable” X ∼ Binomial(n, p) is a
#random variable such that,
#supp(X) = {0, 1, 2, 3, ..., n}
#and that the probabilities are given by,
# 
#n j
#P (X = j) =
#  p (1 − p)n−j
#j
#Use R to write your own script to sample from a Binomial(4, 31 )
#distribution. Note, R already has a built-in feature that makes
#binomial sampling very easy. The p
# --------------------------------------------------------

# n = NUMBER OF TRIALS
# p = probability of success (1)
# Generates a vector of bernoulli trials 1 or 0

mybern = function(n, p) {
  x = runif(n, min=0, max=1)
  r = matrix(NA, nrow=1, ncol=n)
  for (i in 1:n){
    if(x[i] < 1-p){
      r[i] = 0
    } else {
      r[i] = 1
    }
  }
  return(r)
}

# returns P(X=j) for 
p_binom = function(j) {
  N = 1e5
  r = matrix(NA, nrow=1, ncol=N)
  for (i in 1:N){
    bern = mybern(4, 1/3)
    r[i] = sum(bern)
  }
  
  return(sum((r == j))/N)
  
}
# probability of P(X=0)
p_binom(0)


# --------------------------------------------------------
# PROBLEM 2
#Let x be a vector. Write a function in R called
#check.sign which returns the “sign” of each coordinate of x. For
#example, if x = (3, −2, 0, 4, −1) then,
#check.sign(x) = (1, −1, 0, 1, −1)
# --------------------------------------------------------

check.sign = function(x) {
  num_col = ncol(x)
  print(num_col)
  r = matrix(NA, nrow=1, ncol=num_col)
  for (i in 1:num_col){        # looping to check sign
    if(x[i] < 0) {
      r[i] = -1
    } else if(x[i] == 0) {
      r[i] = 0
    } else {
      r[i] = 1
    }
  }
  r
}

example = matrix(c(3,-2,0,4,-1), nrow=1, ncol=5)
check.sign(example)

# =------------------------------------------------------
# PROBLEM 3
# Start at x = 0. After one step either go up by +1
#or go down by −1, with equal probabilities. Thus, after one step
#x = 1 or x = −1 with equal probability. If you are currently at
#x = 1 then either go up by +1 or down by −1, with equal proba-
#  bility. Likewise if you are currently at x = −1. And continue this
#complicated process for 2000 steps.
#Generate a vector x with x[1] = 0 but x[n] denoting the posi-
#  tion after the n-th step. You can visualize the behavior of this
#“symmetric random walk ” by typing,
#plot(1:2000,x,type=’l’)
#What does this random walk resemble?
# --------------------------------------------------------
N = 1e5
r = matrix(NA, nrow=1, ncol=N)
start = 0
r[1] = start
for (i in 2:N) {
  n = runif(1)
  
  if (n < 0.5){
    r[i] = r[i-1] + 1    
  } else {
    r[i] = r[i-1] - 1
  }
  
}


plot(1:N, r,type='l')

# Resembles stock prices
