library(ggplot2)
library(reshape2)

cat("Enter lenght array")
N <- scan(n=1)
cat("Enter flux density")
L <- scan(n=1)
cat("Enter number of implementations: ")
n <- scan(n=1)


arr1 <- matrix(0, ncol = N, nrow = n+1)
arr2 <- matrix(0, ncol = n+1, nrow = N)
a <- (-log(1-runif(N))/L)
arr1[1, ] <- a
for(i in 2:(n+1)){
  a <-rpois(N, L)
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

ggplot(arr, aes(x = X1, y = value, colour = variable)) + geom_line()
