# PROBLEM SET 1 366 Applied Mathematical Computation

# Create a summation using R

n = NULL
i = 10
for (a in 0:i){
  for (j in 0:a) {
    n = c(n, sin(a^2)*cos(j^2))  
  }
}

n
sum(n)

# Create a 90x90 matrix which has 1s on its diagonal
M = matrix(NA, nrow=90, ncol=90)
for (i in 1:90){
  for (j in 1:90){
    if (i == j){
      M[i,j]=1
    } else {
      M[i,j]=0  
    }
  }
}
M


