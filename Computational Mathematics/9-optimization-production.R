

# n is the number of divisions of a full year
# s is storage cost
# r is the amount the firm can produce in tons
# k is the cost of production dollars per ton
# a is the number of additional product
# K is the cost for additional production
n = 4
d = c(100,200, 400, 300)
s = 2
r = 300
k = 20
K = 30
a = 100

optimal.production = function(d,r,k,a,K,s){
  
  objective.in = c(26,24,22,20,36,34,32,30) # linear function coefficients
  #print(objective.in)
  const.mat = rbind(diag(8), diag(8),
                    c(1,0,0,0,1,0,0,0),
                    c(1,1,0,0,1,1,0,0),
                    c(1,1,1,0,1,1,1,0),
                    c(1,1,1,1,1,1,1,1))
  print(const.mat)
  const.dir = c(rep(">=", 8), rep("<=", 8), rep(">=", 3), "=")
  print(const.dir)
  const.rhs = c(rep(0, 8), rep(300, 4), rep(100, 4), 100, 300, 700, 1000) 
  print(const.rhs)
  lp("min", objective.in, const.mat, const.dir, const.rhs)$solution
}

optimal.production(d,r,k,a,K,s)


n=4
c_matrix = matrix(NA, nrow=n, ncol=2*n)
for(i in 1:n){
  for(e in 1:n){
    if (e <= i){
      c_matrix[i,e] = 1
      c_matrix[i,2*e] = 1
    } else {
      c_matrix[i,e] = 0
      c_matrix[i,2*e] = 0
    }
  }
}