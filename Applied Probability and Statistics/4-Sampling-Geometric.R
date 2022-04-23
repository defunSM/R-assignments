# Assignment 4 - MATH 377
# PROBLEM 1
# Twenty dice (six sided) thrown find prob
# of sum of numbers equal to exactly 70

A = matrix(NA, nrow=20, ncol=1e4)
A[1,] = sample(1:6, 1e4, replace=TRUE)

# Writing script to loop to "quickly" fill rows of A
# from row 2 to row 20

for (i in 2:20){
  A[i,] = sample(1:6, 1e4, replace=TRUE)
}

# Creating vector that will sum the column at position i
s = matrix(NA, nrow=1, ncol=1e4)
for(i in 1:1e4){
  s[i] = sum(A[,i])
}

# summing only if the coordinate in the vector is 70
#diving by 1e4 to get prob
prob = sum(s==70)/1e4

# PROBLEM 2
# Geometric distribution X is the waiting time.

# Problematic loop since the first
# success could potentially occur on on the 11th or above
# Since this loop is only being done 10 times.
x=0
for (i in 1:10) {
  if(runif(1) < 0.5){
    x = x+ 1
  } else {
    break
  }
  x = x + 1
}


# While loop geometric to try to fix the approx
# loop for geometric distribution
x = 0
while (TRUE) {
  if(runif(1) < 0.5){
    x = x+ 1
  } else {
    break
  }
  x = x + 1
}

# use ideas from previous problems to generate 1e5
# samples from a geometric(1/2)

# geometric function
geometric = function(p){
  
  x = 0
  
  while (TRUE) {
    if(runif(1) > p){
      x = x+ 1
    } else {
      break
    }
    x = x + 1
  }
  
  return(x)
}
# sampling from geometric
A = matrix(NA, nrow=1, ncol=1e5)
for (i in 1:1e5){
  A[i] = geometric(0.95)
}

hist(A)


# Looping through filling in the matrix with sampels from
# the geom function with p = 1/2
A = matrix(NA, nrow=1, ncol=1e5)
for( i in 1:1e5){
  A[i] = geom(1/2)
}

hist(A)




