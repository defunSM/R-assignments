# Assignment 13


data = rpois(50,lambda = 2)

# Create two different prior beliefs since we have a piecewise function
# IS there a better way to do it? IDK

# Creating the priors
f = function(s){
  if (1 < s & s < 2){
    s-1    
  } else {
    3-s
  }
}

# classical approach
g = function(s){
  -sum(dpois(data, lambda=s), log=TRUE)
}

optim(1, g)

# bayasian approach

# Creating the intervals of our prior distribution functiosn
interval.f = seq.int(from=1, to=3, by=0.01)
post = matrix(NA, nrow=1, ncol=201)

for(i in 1:201){
  post[i] = sum(dpois(data, lambda=interval.f[i], log=TRUE)) + log(f(interval.f[i]))
}

post = exp(post)
post = (post/((0.01)*sum(post)))
post[1] = 0

plot(interval.f, post, type="l")

approx.f = approxfun(interval.f, post)
integrate(approx.f, 1, 3)

interval.f[which.max(post)]

estimate.hat = max(post)

integrate(approx.f, estimate.hat-0.05, estimate.hat+0.05)
