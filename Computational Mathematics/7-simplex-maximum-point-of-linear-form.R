# Lecture related to Oct 7 27:23

# Problem 1: Locate the maximum point (x, y) for the following
#linear form,
#f (x, y) = 0.5x + 1.5y
#subject to the constraints that,
#(i) x + y ≤ 6
#(ii) 3x + y ≤ 15
#(iii) x + 3y ≤ 15
#(iv) x ≥ 0
#(v) y ≥ −1
#Solve this problem by drawing the convex polygon region and
#graphically determining where the maximum occurs. Be careful,
#there is more than one answer!

objective.in = c(0.5, 1.5)

const.mat = rbind(c(1,1),c(3,1),c(1,3),c(1,0),c(0,1))

const.dir = c("<=", "<=", "<=", 
              ">=", ">=")
const.rhs = c(6,15,15,0,-1)

lp(direction = "max", objective.in, const.mat, const.dir, const.rhs)$solution

# Problem 2
#There is an important linear in Linear Algebra, the
#“Cayley-Hamilton Theorem”, which says that if A ∈ Rn×n and if
#p(λ) is the characteristic polynomial of A, i.e.

# Proving that the cayley hamilton theorem always result in a zero matrix
A = rbind(c(1,2), c(2,1))

X =A %*% A - (A[1,1] + A[2,2])*A + (A[1,1]*A[2,2]-A[1,2]*A[2,1])*diag(2)

