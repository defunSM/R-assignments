# nutriention optimization oct 12 46:00


# nutella, sugar, dry rice, peanut butter, whey isolate
cost = c(1.06, 0.37, 0.42, 0.30, 3.08)
p.vec = c(5,0,15,25,84) # amount of protein of each item
c.vec = c(62,100,75,20,8) # amount of carbs in each item
f.vec = c(30,0,1,50,4) # amount of fats in each item
p = 20 # the minimum amount of proteins
c = 30 # the minimum amount of carbs
f = 20 # the minimum amount of fats

optimize.food = function(a, p.vec, c.vec, f.vec, p, c, f, cal.low, cal.high){
  
  calorie.vec = 4*p.vec+4*c.vec+9*f.vec # converting to # of calories
  print(calorie.vec)
  objective.in = a # this is the cost function
  const.mat = rbind(p.vec, c.vec, f.vec, calorie.vec, calorie.vec) # all of the inequalities that need to be satisified
  const.dir = c(rep(">=", 4), "<=") # direction
  const.rhs = c(p,c,f, cal.low, cal.high) # right hand side of the inequalities that need to be satisified.
  
  lp("min", objective.in, const.mat, const.dir, const.rhs)$solution  # outputs the solution 
}

optimize.food(cost, p.vec, c.vec, f.vec, p, c, f, 2000, 2500)
