# Assignment 7
# Problem 1

# ii) Write a function in R called, CDF.norm(t,n), which ap-
# proximates Φ(t) by using the first n terms of the infinite series.
# Calculate CDF.norm(1,5) and compare its value with pnorm(1).
# The two answers should be within 10−4 of each other.

# making a function that approximates the cdf of the normal

CDF.norm = function(t,n){
  # the first case if none of the terms are being used to approximate
  base_approximation = 0.5+1/sqrt(2*pi)*exp(-t/2)
  if (n == 0){
    return(base_approximation)  
  }
  
  value_of_terms = 0
  
  denominator = matrix(NA, nrow=1, ncol=t+1) # values of the denominator for the terms
  denominator[1] = 1
  
  for (i in 1:n){
    denominator[i+1] = (2*i-1)*denominator[i] 
  }
  print(denominator)
  
  for (i in 1:n){
    value_of_terms = value_of_terms+(t^(2*i-1))/denominator[i+1]
  }
  print(value_of_terms)
  
  0.5+(base_approximation-0.5)*value_of_terms  # final approximation for cdf of the norm
}

CDF.norm(1, 5) # 0.8413196
print(pnorm(1)) # 0.8413447

# Problem 2
# i) Let (Ui )1≤i≤n be an independent sequence of Uniform(−1, 1)
# random variables. Construct the random variable,
# The constant α is chosen in such a way to normalize X so that
# Var(X) = 1. Find what α should be equal to? (Note, the choice
# of α will depend on n).
set.seed(1)
n = 1e4
# rewatch vid #9 to figure out from var and expected value why alpha works out
#alpha = 174/n  # need to figure out what alpha should be to normalize X to have var(X) = 1
alpha = sqrt(6/n)
unif_samples = matrix(NA, nrow=1, ncol=n)
for(i in 1:n){
  U = runif(1, min=-1, max=1)
  unif_samples[i] = U
}

X = alpha*unif_samples

# ii)
# checking if X has a variance of 1 we sample from X 
var= sd(X)^2


# Creating function that picks from a sample of X
sample.normal = function(n) {
  X_sample = matrix(NA, nrow=1, ncol=n)
  for (i in 1:n){
    U = runif(n, min=-1, max=1)
    X = alpha*(sum(U))
    
  }
  return(X_sample)
}

sample.norm = function(n){
  Unif = rep(NA, n)
  for (i in 1:n){
    U = runif(1, min = -1, max = 1)
    Unif[i] = U
  }
  norm = sum(Unif) * (sqrt(3/n))
  norm
}
ans = replicate(1e4, sample.norm(8))
sd(ans)

PDF(ans, 20)

# iii) This is not technically the same as replicating 1e4 sample.norm(8)
# still results in a normal distribution
#
set.seed(2)
sample.norm(12)

X_sample = sample.normal(1e4) # this is normally distributed N(0, 1) given the right alpha

print(sd(X_sample)^2) # for some reason 174/n gives an approx var(X) = 1 but no idea why

PDF(X_sample, 20) # normal distribution like expected from CLT approx N(0,1)

# iv)
# dnorm(x) is a quick way to get the pdf of the normal distribution 
curve(dnorm(x), add=TRUE, col="red") # as expected its almost identical as to the real normal




