# Assignment 11
# Generate the data
# November 11

# Problem 1 part 1


L = rexp(20,rate=1) # Typo? in the assignment it says rnorm when we r sampling from exponential

mle = 20/sum(L) # mle of the exponential distribution from 1

f = function(p){
  -sum(dexp(L, rate = p, log = TRUE))
}

optim(3,f)$par


# part 2 
# We start with creating the intervals
set.seed(20)
p = seq.int(from=0.0, to=10.0, length.out=21)

prior = function(p) { 1/10 }

PDF(p, 20) # Confirm this is our prior distribution (uniform)

# Generate our posterior distribution
post = rep(0, 21) # same number of intervals as the prior
for(i in 2:20){
  post[i] = prod(dexp(L, rate=p[i]))*prior(p[i]) # Need to understand what is going on here
}

# NORMALIZING by the subinterval length
post = post/((0.5)*sum(post)) # NOT EXACT BUT GOOD ENOUGH normalizing const
plot(p, post, type="l")

# approximating the posterior distribution function so we can integrate between
# the mle confidence interval given in the problem
f = approxfun(p, post)
integrate(f, lower=20/sum(L)-0.1, upper=20/sum(L)+0.1) # Confidence interval

# Below are sanity checks

integrate(f, lower=0, upper=10) # making sure area under curve is actually 1 that our posterior is a probability density function

optimize(f, interval=c(0,10), maximum=TRUE) # finding the maximum aka most likely value for rate from posterior distribution

# part 3
# pretty much copy and paste from above just change to expoential instead of uniform prior distribution
# finish up part 3

prior.exp = matrix(NA, nrow=1, ncol=20)
prior.exp[1] = 0
for( i in 2:101){
  prior.exp[i] = prior.exp[i-1] + 1/10 
}

prior.exp = matrix(NA, nrow=1, ncol=101)

p = seq.int(from=0, to=10, by=0.5)

post = rep(0, 21)
for(i in 2:21){
  post[i] = prod(dexp(L, rate=p[i]))*dexp(p[i])
}

post = post/((0.5)*sum(post))
plot(p, post, type="line")
f = approxfun(p, post)
integrate(f, lower=mle-0.1, upper=mle+0.1)
