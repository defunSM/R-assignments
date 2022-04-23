# Assignment 10

# Use optim to estimate the probability parameters of a multinomial distribution
# this is done by using optim 

# construct the sample
x = rmultinom(1e3, 24, prob= c(0.1,0.2,0.3,0.4))


L = function(p){
  -sum(dmultinom(x[,1], size=24, prob=c(p[1],p[2],p[3],p[4]), log=TRUE)) 
  # dont forget the negative infront to reverse since by default optim finds the 
  # minimum
  
  # also set logg to TRUE to take log likihood
}

optim(c(0.1,0.2,0.3,0.4), L)$par # the actual probabilities used to generate
# the sample from the multinomial distribution and then approximatiing the
# parameters using the maximum log likelihood estimate

# The above only estimates parameter using 1 sample now the below will use all
# samples but there are various ways to approach this
estimates = matrix(NA, nrow=1e3, ncol=4)
for (i in 1:1e3){
  L = function(p){
    -sum(dmultinom(x[,i], size=24, prob=c(p[1],p[2],p[3],p[4]), log=TRUE)) 
  }
  
  estimates[i,] = optim(c(0.1,0.2,0.3,0.4), L)$par
}
mean(estimates)