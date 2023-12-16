X = c(3, 1, 2, 3, 1, 4, 3, 3)
X[1:8]
n = 8
Mp = mean(X[1:n-1])
Mp
((n-1)*Mp + X[n])/n
mean(X)

n= 8
m = 4
Ml = mean(X[1:m])
Mr = mean(X[(m +1):n])

(n - m) * Mr
(m*Ml + (n-m)* Mr) / n
