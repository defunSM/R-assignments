# Assignment 2 377 Applied Prob and Stats
# ---------------------------------------------------------------------
# PROBLEM 1:
# Take the linear function f (x) = 5x + 1, and start the sequence
# with x_1 = 1. Generate the sequence (x_n )1≤n≤105 where
#                   x_n = f (x_n−1 ) mod 2^(20)

#Note, to normalize this sequence so it is contained in the interval
#(0, 1) we divide out by 2^20 . Let (y_n )1≤n≤105 denote this normalized
#sequence.

#This normalized sequence will give you a RNG. Let us take a small
#segment of this sequence, consider rather the sub-sequence,
#(y_n )500≤n≤1000

#(i) What is the average (mean) of all those 501 numbers? What
#do you expect the answer to be? What is the actual mean?
# ---------------------------------------------------------------------

# including for repeatable random sequences
set.seed(1) 

# Creating the function f
f = function(x) {
  5*x+1 
}

# Creating the loop for vector containing all the numbers
x = 2
for (i in 1:1e5){
  x = c(x, f(x[i])%%2^20)   
}
x

# Normalized seqence
y = x/2^20

# sub-sequence y_n 500 <= n <= 1000

# ANSWER to (i)
# The expected value for a uniform distribution is the midpoint of the intervals
# thus the midpoint is 1/2 for our interval of [0,1] which is what we are expecting
mean(y[500:1000]) # Outputs 0.505855

# As expected we got something close 1/2. If we used the entire sequence it 
# would probably be a better approximation

# ANSWER to (ii)
# Variance of a uniform random variable is (b-a)^2/12
# so the standard deviation would be sqrt(1/12) aprox = 0.2886
sd(y[500:1000]) # Outputs 0.2814

# As expected the answer is close to what we expected from the uniform distribution

# ---------------------------------------------------------------------
# PROBLEM 2: 
# Suppose you have a RNG that produces a pseudo-
# random sequence contained in the interval (0, 1). How can you
# easily modify this RNG so that it will produce an evenly (uni-
# formly) distributed sequence but rather contained in the interval
# (−1, 1)?
  
#Basically, if (x_n )n≥1 is evenly distributed throughout (0, 1) how can
#you use that sequence to build a new sequence that will now allow
#negative numbers and be evenly distributed throughout (−1, 1)?
# ---------------------------------------------------------------------

# creating seq from 0 to 1
s = runif(min=0, max=1, n=1e5)

# ANSWER to 2
# Method to modify the interval of the seq to (-1,1)
# multiplied the original seq with 2 and subtracted 1.
m_s = (s*2-1)
hist(m_s)

# we can check if this new seq m_s is uniform
sum(m_s < 0) # Outputs 50103

# This is pretty much what we expect since this is half the interval
# so half os the sample points are within it for the uniform distribution
# is as expected.



