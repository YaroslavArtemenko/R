library(ggplot2)
library(reshape2)

cat("Enter lenght array")
N <- scan(n=1)
cat("Enter flux density")
L <- scan(n=1)
cat("Enter number of implementations: ")
n <- scan(n=1)

cat("Enter k (number of occurrent): ")
k <- scan(n=1)

cat("Enter m (number of occurrents): ")
m <- scan(n=1)

arr1 <- matrix(0, ncol = N, nrow = n+1)
arr2 <- matrix(0, ncol = n+1, nrow = N)
a <- (-log(1-runif(N))/L)
arr1[1, ] <- a
for(i in 2:(n+1)){
  a <- rpois(N, L)
  arr1[i, ] <- a
  
  
}
for(i in 1:(n+1)){
  sum = 0
  for(j in 1:N){
    sum = sum +arr1[i,j]
    arr2[j,i] = sum
    
  }
}


arr <- melt(data = data.frame(arr2), id.vars = "X1")
t <- matrix(0, ncol = round( N*L/k,0), nrow = 1)
mn<- matrix(0, ncol = round( N*L/k,0), nrow = 1)


hist(arr1[1,])

j=1
tsum=0
for(i in 1:(N)){
 if((k*j)<=arr[i,3]){
   t[1,j] = arr[i,1]-tsum
   tsum=tsum+t[1,j]
   j=j+1
 }
}
hist(t)

j=1
mnsum=0
for(i in 1:(N)){
  if((m*j)==arr[i,3]){
    mn[1,j]=( arr[i,1]-mnsum)
    mnsum=mnsum+mn[1,j]
    j=j+1
  }
}
mn1<- matrix(0, ncol =j, nrow = 1)
for(i in 1:j){
  mn1[1,j]=mn[1,j]
}
hist(mn1)
