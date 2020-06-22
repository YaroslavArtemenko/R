cat("Enter T")
T <- scan(n=1)
cat("Enter quantity of intervals")
N <- scan(n=1)
cat("Enter quantity of implementations")
n <- scan(n=1)

h <- T/N
96

t <- array(0,N+1)
for( i in 1:(N+1)){
  t[i] = (i-1)*h
}

X <- matrix(0, n, N+1)

for(i in 1:n){
  ksi <- runif(1)
  W <- rnorm(N+1, T/2,T/2)
  X[i, 1] = ksi
  for(j in 1:N+1){
    X[i, j] = X[i, j-1]+ h*(X[i,j-1]+(W[j]-W[j-1]))
  }
plot(X[i,])
}
meanX <-matrix(0, nrow=1, ncol=N+1)
for(i in 1:N+1){
  meanX[1,i] = sum(X[,i])
}
plot(meanX[1,])