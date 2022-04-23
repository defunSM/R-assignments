# Assignment 8

# Problem 1
#In the US population the average adult male popu-
#lation is normally distributed with a mean of 69 inches and with a
#standard deviation of 2.9 inches. For the adult female population
#the distribution is with a mean of 64 inches and with a standard
#deviation of 2.7 inches.

#(i) Suppose a random male and a random female are chosen
#from the population determine the probability that the female
#will be taller than the male. Solve this problem mathematically.
#(Hint: The inequality F > M and be rewritten as F − M > 0.
#  Now use the well-familiar fact that a linear combination of inde-
#    pendent normals is normal.)

# should be about ~.10

#ii) (ii) Solve this problem by running a simulation. Draw indepen-
#  dent samples from the two normal distributions and find how fre-
#  quently the female population exceeds the male population. Com-
#  pare your two answers.

# Create two different normal r.vs
# M ~ N(69, 2.9^2), W ~ N(64, 2.7^2)

set.seed(1)

n = 1e4
M = rnorm(n, mean=69, sd=2.9)  # The men rv
W = rnorm(n, mean=64, sd=2.7)  # women rv

P = matrix(NA, nrow=1, ncol=n) # storing 0 or 1 depending if W is greater then M
for (i in 1:n){
  if (W[i] > M[i]){
    P[i] = 1             # 1 if W>M
  } else {
    P[i] = 0            # 0 if W<M
  }
}

# this is the probability that P(F>M) or P(F-M>0) mathematically
prob = sum(P)/n # as epected it is close to the mathematical answer 0.1038



