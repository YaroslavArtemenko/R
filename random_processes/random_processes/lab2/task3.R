library(ggplot2)
library(reshape2)


N <- 17000

ksi_1 <- function(t,N){
  sum_ksi <- 0
  for(i in 1:N){
    a <- rnorm(1, 0, 1/(1+i**4)**2)
    b <- rnorm(1, 0, 1/(1+i**4)**2)
    sum_ksi = sum_ksi + cos(i*pi*t) * a + sin(i*pi*t) * b
  }
  return(sum_ksi)
}

ksi_2 <- function(t,N){
  sum_ksi <- 0
  for(i in 1:N){
    a <- rnorm(1, 0, 1/(1+i**4)**2)
    b <- rnorm(1, 0, 1/(1+i**4)**2)
    lambda <- runif(1,min = i*pi,max = (i+1)*pi)
    sum_ksi = sum_ksi + cos(lambda*t) * a + sin(lambda*t) * b
  }
  return(sum_ksi)
}

cat('Enter the number of implementations : ')
num_implementations <- scan(n=1)
cat('Enter the number of breakdowns : ')
len = scan(n=1)

gaus_1 <- matrix(0, ncol = 1+num_implementations, nrow=len)
gaus_2 <- matrix(0, ncol = 1+num_implementations, nrow=len)
gaus_mean_a <- matrix(0, ncol = 2, nrow=len)
gaus_mean_b<- matrix(0, ncol = 2, nrow=len)

t <- seq(0, pi, length=len)
gaus_1[,1] = t
gaus_2[,1] = t
gaus_mean_a[,1] = t
gaus_mean_b[,1] = t

for(j in 2:(num_implementations+1)){
  for(i in 1:len){
    gaus_1[i, (j)] = ksi_1(t[i], N)
  }
  for(i in 1:len){
    gaus_2[i, (j)] = ksi_2(t[i], N)
  }
}

for(i in 1:len){
  a <- gaus_1[i,]
  b <- gaus_2[i,]
  gaus_mean_a[i, 2] = mean(a[2:length(a)])
  gaus_mean_b[i, 2] = mean(b[2:length(b)])
}

a <- data.frame(gaus_1)
a1 <- melt(data = a, id.vars = "X1")

b <- data.frame(gaus_2)
b1 <- melt(data = b, id.vars = "X1")

a_mean <- data.frame(gaus_mean_a)
a1_mean <- melt(data = a_mean, id.vars = "X1")

b_mean <- data.frame(gaus_mean_b)
b1_mean <- melt(data = b_mean, id.vars = "X1" )

ggplot(a1, aes(x = X1, y = value, colour = variable)) + geom_line()
ggplot(data = b1, aes(x = X1, y = value, colour = variable)) + geom_line()
ggplot(data = a1_mean, aes(x = X1, y = value, colour = variable)) + geom_line()
ggplot(data = b1_mean, aes(x = X1, y = value, colour = variable)) + geom_line()


