# assignment 12
# Problem 1

x = runif(50,min=-1,max=1)
y = runif(50,min=-1,max=1)
r = sqrt(x*x+y*y)
data = rep(0,50)
for(i in 1:50){
  if(r[i] < 1){
    data[i] = 1
  }
}



prior = function(p){48*(p-1/2)*(1-p)}

# You can not write all possible values of p instead u sub divide the interval
# (1/2,1) into many sub intervals na calculate how likely it is to
# generate the data using bayes theorem for each of those values of p

p = rep(NA, 51)
p[1] = 0.5
for ( i in 2:51){
  p[i] = p[i-1] + 0.01
}

p = seq.int(from=0.5, to=1.0, length.out=51)

# Now we compute "posterior distribution". This is the probability of a
# possible value of p being correct, given that you have this is
# computed using bayes theorem

post = rep(0,51)
for (i in 1:51) {
  post[i] = prod(dbinom(data, size=1, prob=p[i])*prior(p[i]))
}


post = post/(0.01)*sum(post)
plot(p, post, type="l")

# finding the maximum

# checking if integrates to 1

f = approxfun(p,post)
# find the maximum
df = curve(f, from=0.5, to=1, add=TRUE, col="red")
df$x[which(df$y==(max(df$y,na.rm = TRUE)))]

integrate(prior, lower=0.5, upper=1.0)


