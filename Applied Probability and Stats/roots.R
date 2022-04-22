locate.root = function(f,a=0,b=1,n=20){
root = (a+b)/2 # choose the midpoint as your initial root approximation
for(i in 1:n){
if( f(a)*f(root) < 0 ){
b = root
root = (a+b)/2
}
else{
a = root
root = (a+b)/2
}
}
root #final output result
}

roots = function(f,a=0,b=1,N=20,n=20){
x = rep(NA,N+1)
roots = NULL
for( i in 1:(N+1) ){
x[i] = a + (b-a)/N*(i-1)  # vector of sub-interval points
}
for( i in 1:N ){
if( f(x[i])*f(x[i+1]) < 0 ){  # check for a change in sign
roots = c(roots , locate.root(f,x[i], x[i+1], n) )
# apply bisection algorithm on x[i] to x[i+1]
}
}
roots # final output
}

