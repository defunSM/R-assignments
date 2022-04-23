# PROBLEM 1 Consider the following sequence X_n where n >= 1
# where X_n = |sin(n)|
# i) explain why this sequence is contained inside the interval of (0,1)
# This is because sin(n) has a range between -1 to 1 and with the 
# absolute value the numbers must be non negative thus leaving the interval
# of 0 to 1

# ii) Construct a vector in R of the fiirst 1e5 terms of the sequence

x = NULL
for (i in 1:1e5){
  x <- c(x, abs(sin(i)))
}
print(x)
# iii) Does this sequence appear to be uniformly distributed?
# using the hist() we can see that it is clearly not uniformly distributed
# as majority of the values seem to be closer to 1 than 0. 
# i.e lim n -> inf c(n)/n = len(J) J being the interval that you count the values are between
# but in this situation that does not appear
# to be the case as checking the interval of 0 to 1/2 we count 33327 which is
# not very close to 50,000.
hist(x)

sum((x < 1/2)) # outputs 33327
(1/2)*1e5 # the expected answer was 50,000 if this was uniform

# iv) depending on how you answered v) can you give an explanation
# why your answer is expected?
# sin is oscillating between -1 and 1 thus is not truely random thus
# can not produce a uniform distribution if the function can not generate
# a random sequence of numbers.

# PROBLEM 2: Fix a rational # q. Consider the sequence
# (X_n) where n >= 1
# where X_n = qn-floor(qn)

q = 42.123
a = NULL
for (n in 1:1e4){
  a <- c(a, q*n-floor(q*n))  
}
print(a)
hist(a)

# This seq is not random because the sequence are all multiples of n and thus
# can be the sequence is not random such as ones generated from irrational
# numbers.
sum((a < 1/2))
