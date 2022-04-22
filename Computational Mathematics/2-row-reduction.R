# Assignment 2 - 366 Intro Computational Mathematics
#------------------------------------------------------------
# PROBLEM 1:
# In class we created a function called
# pivot.below(A,row,col) which takes a matrix A and uses the
# matrix position at the specified row and col position to fill all the
# rows with 0’s. Note, the code leads to an ERROR if row is equal
# to the largest possible row number. More specifically, if n is the
# number of rows of A then row< n. Otherwise we will get an error
# if row= n.

# Consider the following code,
# row.reduce = function(A){
#  n = nrow(A)
#  for(i in 1:(n-1)){
#    A = pivot.below(A,i,i)
#  }
#  A
#}

# Note, the loop runs from 1 to n − 1 to avoid the error with the
# pivot code. This code is incomplete. The last row of the output
# matrix is not reduced properly. Fix the code by adding an extra
# line for the last row so that the resulting output matrix is correctly
# row reduced.
# -------------------------------------------------------------

pivot.below = function(A, r, c) {
  n = nrow(A) # get the rows of A
  A[r,] <- 1/A[r,c]*A[r,] # first create a 1 in a specified pivot position
  n_f = r+1              # Fixed this line here as R has some unintuitive splicing behavior
  for(i in n_f:n){
    A[i,] = A[i,] + (-A[i,c])*A[r,]
  }
  
  A
}
# The behavior of 1+1:5-1 intuitively would be 2:4 however R seems to eval
# 1+1:5-1 as 1:5 meaning R recognizes the addition and subtraction and cancels
# them out. This would indicate that : has higher precendence then operators like
# +, - in R. While this seems reasonable and maybe even mathematically sound to do
# it leads to "unintuitive" behavior causes a out of bounds error from a unexpected
# sequence. Which can be resolved by adding line 31 is necessary to be outside 
# of the for loop changing the sequence being generated. This behavior is different
# from how python uses :

row.reduce = function(A){
  n = nrow(A)
  for(i in 1:(n-1)){
    A = pivot.below(A,i,i)
  }
  return(A)
}

A = matrix(c(1,2,3,2,3,4,3,4,5), nrow=3, ncol=3)

pivot.below(A,1,1)

row.reduce(A)

# -----------------------------------------------------------------------------
# PROBLEM 2: Write a code called pivot.above(A,row,col) which
# does the pivoting procedure but rather it fills 0 above the diagonal
# of 1’s.
# -----------------------------------------------------------------------------

pivot.above = function(A, r, c) {
  n = nrow(A) # get the rows of A
  A[r,] <- 1/A[r,c]*A[r,] # first create a 1 in a specified pivot position
  n_f = r-1              # Changed this from r+1 to r-1 going above
  for(i in n_f:1){         # going from the top to bottom so should end with 1
    A[i,] = A[i,] + (-A[i,c])*A[r,]
  }
  
  A
}
A = matrix(c(1,2,3,2,3,4,3,4,5), nrow=3, ncol=3)
pivot.above(A,3,1)

# -----------------------------------------------------------------------------
# Problem 3: Write a code called full.row.reduce(A) which
# completes row reduction and fully reduces the matrix instead of
# just reducing the numbers below the diagonal.
# -----------------------------------------------------------------------------

# Helps to make the above numbers zero trying to get an identity matrix if possible
row.reduce.above = function(A, r, c){
  n = nrow(A)
  for (i in r-1:1){
    A[r-i,c] = A[r-i,c] - (A[r,c]*A[r-i,c])
  }
  return(A)
}

# IGNORE THIS FUNCTION NOT USED
row.reduce.echelon = function(A){
  n = nrow(A)
  c = ncol(A)

  for (i in 1:n){
    for (j in 1:c) {
      A[n-i,c] = A[i,j]-((A[i,j])*A[i,j])
    }
  }
  return(A)
}

# Final function utilizing row.reduce, row.reduce.above
full.row.reduce = function(A) {
  n = nrow(A)
  A = row.reduce(A) # Initial row reduce
  print(A)
  for (i in n:2) {
    A = row.reduce.above(A, i, i)  # simulating backsubstition
  }
  return(A)
}

A = matrix(c(1,2,3,2,3,4,3,4,5), nrow=3, ncol=3)
full.row.reduce(A)


