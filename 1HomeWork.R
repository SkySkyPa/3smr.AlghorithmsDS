X = c(3, 1, 2, 3, 1, 4, 3, 3)
Y = c(1, 4, 3, 1, 1, 3)
Z = c(3, 3, 1, 4, 2, 1, 4, 2)

# mean a

mean(X)
mean(Y)
mean(Z)

# median b

median(X)
median(Y)
median(Z)

# mode c

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(X)
find_mode(Y)
find_mode(Z)

# geometric mean d

gmean <- function(X) {
  p = 1
  for (x in X) {
    p = p * x
  }
  return(p^(1/length(X)))
}

gmean(X)
gmean(Y)
gmean(Z)

# harmonic mean e

hmean <- function(X) {
  s = 0
  for (x in X) {
    s = s + 1/x
  }
  return(length(X)/s)
}

hmean(X)
hmean(Y)
hmean(Z)

# arithmetic-geometric mean f

amean <- function(X) {
  s <- 0
  for (x in X) {
    s <- s + x
  }
  return(s/length(X))
}

agmean <- function(X) {
  am <- amean(X)
  gm <- gmean(X)
  while (abs(am-gm) > 0.000001){
    tmp <- (am + gm)/2
    gm <- (am * gm)^(1/2)
    am <- tmp
  }
  return((am + gm)/2)
}

agmean(X)
agmean(Y)
agmean(Z)

# arithmetic-harmonic-geometric mean g

aghmean <- function(X) {
  am <- amean(X)
  gm <- gmean(X)
  hm <- hmean(X)
  while (abs(am-gm) > 0.000001 | abs(am-hm) > 0.000001| abs(gm-hm) > 0.000001){
    tmpa <- amean(c(am, gm, hm))
    tmpg <- gmean(c(am, gm, hm))
    hm <- hmean(c(am, gm, hm))
    am = tmpa
    gm = tmpg
  }
  return((am + gm + hm)/3)
}

aghmean(X)
aghmean(Y)
aghmean(Z)

# variance h

var(X)
var(Y)
var(Z)

# standard deviation i

sd(X)
sd(Y)
sd(Z)

# MAD around mean j

MADm = function(X) {
  s = 0
  m = amean(X)
  for (x in X){
    s = s + abs(x - m)
  }
  return(s/length(X))
}

MADm(X)
MADm(Y)
MADm(Z)

# MAD around median k

MADmed = function(X) {
  s = 0
  m = median(X)
  for (x in X){
    s = s + abs(x - m)
  }
  return(s/length(X))
}

MADmed(X)
MADmed(Y)
MADmed(Z)

# MAD around mode l

MADmod = function(X) {
  s = 0
  m = find_mode(X)
  for (x in X){
    s = s + abs(x - m)
  }
  return(s/length(X))
}

MADmod(X)
MADmod(Y)
MADmod(Z)

# interquartile range n 

quantile(X)
iqrX = 3.00 - 1.75 # Q3 - Q1
iqrX

IQR(Y)

IQR(Z)

# quartile based skewness o

# (Q3 + Q1 - 2ui) / (Q3 - Q1)

quantile(X)
qbsX = (3.00 + 1.75 - 2 * mean(X)) / (3.00 - 1.75) 
qbsX

quantile(Y)
qbsY = (3 + 1 - 2 * mean(Y)) / (3 - 1)
qbsY

quantile(Z)
qbsZ = (3.25 + 1.75 - 2 * mean(Z)) / (3.25 - 1.75)
qbsZ

# Pearson's first skewness p

(mean(X) - find_mode(X))/ sd(X)
(mean(Y) - find_mode(Y))/ sd(Y)
(mean(Z) - find_mode(Z))/ sd(Z)

# Pearson's second skewness q

((mean(X) - median(X))* 3)/ sd(X)
((mean(Y) - median(Y))* 3)/ sd(Y)
((mean(Z) - median(Z))* 3)/ sd(Z)

# Groeneveld Meeden's coeff r

(mean(X) - median(X))/sum(X - median(X))
(mean(Y) - median(Y))/sum(Y - median(Y))
(mean(Z) - median(Z))/sum(Z - median(Z))

# Pearsons moment coeff s

sum((X - mean(X))^ 3 / var(X))
sum((Y - mean(Y))^ 3 / var(Y))
sum((Z - mean(Z))^ 3 / var(Z))

# the Kurtosis t

library(moments)

kurtosis(X)
kurtosis(Y)
kurtosis(Z)
