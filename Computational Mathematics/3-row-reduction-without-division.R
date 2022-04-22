# Assignment 3 - MATH 366

#---------------------------------------------------------------------------
# PROBLEM 1:
#Sometimes a different type of row reduction is more
#useful. Usually our pivots are all equal to 1. It is possible to row
#reduce the matrix by leaving the pivots alone. Here is a worked
#out example with a 3 × 3 matrix that illustrates the procedure.
#Suppose we wish to row reduce the following matrix,
#
#
#2 3 4
#5 6 7
#8 9 1
#
#Usually we begin by replacing R1 7→ 12 R1 so that the pivot in the
#upper-left corner, i.e. 2, turns into 1. But we can leave the pivot
#alone and proceed to pivot below that 2 and turn the 5 and 8 into
#0’s. In this case we do the row operations,

#−( 25 )R1 + R2 7→ R2
#and − 4R1 + R3 7→ R3
#The resulting matrix becomes,
#2 3
#4
#0 − 3 −3 
#2
#0 −3 −15
#So on and so forth until the matrix is row reduced.

#---------------------------------------------------------------------------
swaprows = function(A, r1, r2){
  temp = A[r1, ]
  A[r1,] = A[r2,]
  A[r2,] = temp
  return(A)
}

pivot.below.v2 = function(A, r, c) {
  n = nrow(A) # get the rows of A
  #A[r,] <- A[r,]/A[r,c] # first create a 1 in a specified pivot position
  n_s = r+1
  for(i in n_s:n){
    A[i,] = A[i,] + (-A[i,c]/A[r,c])*A[r,]  
  }
  A
  return(A)
}

example = matrix(c(1,5,9,2,6,10,3,7,11,4,8,12), nrow=3, ncol=4)
example
example[2,]

row.reduce.nd = function(A){
  n = nrow(A)
  c = ncol(A)-2
  for (i in 1:c){
    A = pivot.below.v2(A, i, i)
  }
  A
}

row.reduce.nd(example)
det(example)


#---------------------------------------------------------------------------
# PROBLEM 2:
#---------------------------------------------------------------------------

 