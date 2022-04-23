# PROBLEM 1
# iii)

A = matrix(NA, nrow=1e5, ncol=3)

covariance_matrix = rbind(c(6,12,4),
                          c(12, 29,5),
                          c(4,5,12))

for(i in 1:3){
  A[,i] = rnorm(1e5)
}

# Transformed Y
B = matrix(NA, nrow=1e5, ncol=3)

for(i in 1:1e5){
  B[i,] = covariance_matrix %*% A[i,]
}

# Problem 2: Approximate the m and Σ parameter by calculating
#the average of the data and finding the covariance matrix. Com-
#  pare your answers with what they should be equal to.

# Approximating m
mean(B[,1]) # -0.06
mean(B[,2]) # -0.15
mean(B[,3]) # 0.04

# The m vector is approx (0,0,0) as expected since Y will be
# distributed as normal(0, Σ ) using transformation g(x) = Σx

# calculating covariances to approx covariance matrix for Y
cov(B[,1], B[,1])
cov(B[,1], B[,2])
cov(B[,1], B[,3])
cov(B[,2], B[,1])
cov(B[,2], B[,2])
cov(B[,2], B[,3])
cov(B[,3], B[,1])
cov(B[,3], B[,2])
cov(B[,3], B[,3])

# This shows that it is a covariance matrix sine it is positive definite matrix
# However not sure if the values are what they should be


# PROBLEM 3
within_unit_ball = matrix(NA, nrow=1, ncol=1e5)
for (i in 1:1e5){
  if (A[i,1] <= 1 & A[i,2] <= 1 & A[i,3]){
    within_unit_ball[i] = 1
  } else {
    within_unit_ball[i] = 0
  }
}

sum(within_unit_ball)/1e5  # 70% chance of being within the unit ball

#Check answer with built in mvrnorm
norm_matrix = mvrnorm(1e5, c(0,0,0), diag(3))
for (i in 1:1e5){
  if (norm_matrix[i,1] <= 1 & norm_matrix[i,2] <= 1 & norm_matrix[i,3]){
    within_unit_ball[i] = 1
  } else {
    within_unit_ball[i] = 0
  }
}
sum(within_unit_ball)/1e5  # also a 70% chance so probably did it right
