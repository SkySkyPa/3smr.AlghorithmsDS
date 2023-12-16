2 + 3
2^3
log(2)
log(4)/log(2)

#  Variable and Assignment

x = 5
y = 6
x + y

# Arrays

X = c(3, 1, 2, 3, 1, 4, 3, 3)
X[2]

rm(X)    #  to delete
X = c(3, 1, 2, 3, 1, 4, 3, 3)


d = length(X)
X[5:8]
X2 = X[1:(d-1)]
X[-d]

# For loop  

#  summation 
s = 0
for (x in X) {
  s = s + x
}
s





#  product
p = 1
for(i in 1:d){
  p = p * X[i]
}
p





#  while loop
s = 0
ix = 0;
while (ix < d){
  ix = ix + 1
  s = s + X[ix]
}
s





#  function
sumall = function(X) {
  s <- 0
  for (x in X) {
    s = s + x
  }
  return(s)
}

sumall(X)






#  view  -> 
#  move focus to Source
prodall = function(X) {
  m = 1
  for (x in X) {
    m = m * x
  }
  return(m)
}

S = sort(X)

Hx = c(2, 2, 2, 2)
max(Hx)
which.max(Hx)

agmeanR = function(am, gm){
  if (abs(am-gm) < 0.000001){
      return((am + gm)/2)
  else{
    
  }
}

#  clear and close all
To clear the console,  cnt + L
To clear the memory, Misc "Remove all Objects"
