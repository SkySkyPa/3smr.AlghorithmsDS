# Q 3.7

# a

meanonline <- function(xn, n, pm){
  return(((n - 1) * pm + xn)/n)
}
meanonline(0.03, 44, -0.07860465)

# b 

varonline <- function(xn, n, pm, pv){
  m = meanonline(xn, n, pm)
  return(((n - 1) * (pv + pm^2) + xn^2)/n - m^2)
}
varonline(0.03, 44, -0.7860465, 0.07611898)

# c

SMAonline <- function(xn, xk, pm, k){
  return(pm + (xn - xk)/k)
}
SMAonline(0.03, 0.09, 0.1185, 20)

# d

SMVonline <- function(xn, xk, pm, pv, k){
  m = SMAonline <- function(xn, xk, pm, k)
  return(pv + (xn^2 - xk^2)/k - m^2 + pm^2)
}
SMVonline(0.03, 0.09, 0.1185, 0.04694275, 20)
