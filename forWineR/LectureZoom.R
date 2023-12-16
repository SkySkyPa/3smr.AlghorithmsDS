library(readr)
wineR <- read_csv("forWineR/wineR.csv")
View(wineR)

install.packages("ramify") 

library(ramify)

R = mat("1, 2, 1; 
3, 3, 1; 
5, 4, 1; 
6, 4, 1; 
2, 1, 2; 
7, 2, 2; 
6, 1, 2")

dim(R)

nr = dim(R)[1]
nc = dim(R)[2]

d = nc - 1
R[1:4,-nc]  
R[5:7,-nc]  


q = mat("6, 2")
r = R[1,1:2]

p = 1.6
(sum((abs(q - r))^p))^(1/p)

sqrt(sum((q - r)^2))

q = mat("3, 1")

(sum((abs(q - r))^p))^(1/p)

sqrt(sum((q - r)^2))

L2q = array(0, dim=c(nr,1))
L2q

for(i in 1:nr){
  L2q[i] = sqrt(sum((q - R[i,1:d])^2))
}
L2q

q

which.min(L2q)

if (which.min(L2q) > 4) {
  NNc = 2
} else {
  NNc = 1
}
NNc

NNclassifier <- function(q, R) {
  nr = dim(R)[1]
  nc = dim(R)[2]
  d = nc - 1
  L2q = array(0, dim=c(nr,1))
  for(i in 1:nr){
    L2q[i] = sqrt(sum((q - R[i,1:d])^2))
  }
  return(R[which.min(L2q),nc])
}

NNclassifier(q, R)

NNclassifier(mat("6, 2"), R)

#  kNN
k = 3
T = order(L2q)

V = array(0, dim=c(1,max(R[,nc])))
for(i in 1:k){
  V[R[T[i],nc]] = V[R[T[i],nc]] + 1
}
which.max(V)

kNNclassifier <- function(q, R, k) {
  nr = dim(R)[1]
  nc = dim(R)[2]
  d = nc - 1
  L2q = array(0, dim=c(nr,1))
  for(i in 1:nr){
    L2q[i] = sqrt(sum((q - R[i,1:d])^2))
  }
  T = order(L2q)
  V = array(0, dim=c(1,max(R[,nc])))
  for(i in 1:k){
    V[R[T[i],nc]] = V[R[T[i],nc]] + 1
  }
  return(which.max(V))
}

kNNclassifier(q, R, 3)

kNNclassifier(mat("6, 2"), R, k)

T[1]
L2q[T[3]]

w0 = 0
w1 = 0
for(i in 1:k){
  if (T[i] > 4){
    w1 = w1 + 1/(L2q[T[i]]^2)
  } else {
    w0 = w0 + 1/(L2q[T[i]]^2)
  }
}

if (w0 < w1) {
  wkNNc = 1
} else {
  wkNNc = 0
}

wkNNc

wkNNclassifier <- function(q, R, k) {
  nr = dim(R)[1]
  nc = dim(R)[2]
  d = nc - 1
  L2q = array(0, dim=c(nr,1))
  for(i in 1:nr){
    L2q[i] = sqrt(sum((q - R[i,1:d])^2))
  }
  T = order(L2q)
  
  
  V = array(0, dim=c(1,max(R[,nc])))
  for(i in 1:k){
    V[R[T[i],nc]] = V[R[T[i],nc]] + 1/(L2q[T[i]]^2)
  }
  return(which.max(V))
}


wkNNclassifier(q, R, 3)

Rn0 = R
for(i in 1:nr){
  Rn0[i,1] = (Rn0[i,1] - cp)/rangemx
}
Rn0


Rn0 = R
for (j in 1:d){
  rangemx = max(R[,j]) - min(R[,j])
  cp = min(R[,j])
  for(i in 1:nr){
    Rn0[i,j] = (Rn0[i,j] - cp)/rangemx
  }
}
Rn0

qn0 = (q - cp)/rangemx

kNNclassifier(qn0, Rn0, 3)

getwd()
setwd("/Users/aristarcharistarchovich/DataspellProjects/3smr.AlghorithmsforDS")
data <- read.csv("iris.csv")
print(data)

library(readr)
data <- read_csv("forWineR/iris.csv")

A = as.matrix(data)

nr = dim(A)[1]
nc = dim(A)[2]

R = zeros(90, 5)

for(i in 1:30){
  R[i,] = A[i+20,]
}
R

for(i in 1:30){
  R[i+30,] = A[i+70,]
}
R

for(i in 1:20){
  R[i+60,] = A[i+120,]
}
R
