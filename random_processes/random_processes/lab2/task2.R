library(nortest)

ksi_f <- function(N){
  a <- runif(N)
  b <- runif(N)
  ksi <- array(0, 2*N)
  for(i in 1:N){
    ksi[2*i-1] = sqrt(-2*log(a[i]))*sin(2*pi*b[i])
    ksi[2*i]   = sqrt(-2*log(a[i]))*cos(2*pi*b[i])
  }
  return(ksi)
}

ksi_i <- function(N, n){
  ksi <- array(0, N)
  for(i in 1:N){
    for(j in 1:n){
      ksi[i] = ksi[i] + (runif(1)-0.5)
    }
    ksi[i] = sqrt(12/n)*ksi[i]
  }
  return(ksi)
}

cat("Enter the lenght of array of : ")
N <- scan(n=1)

a <- rnorm(N)
a_f <- ksi_f(N)

hist(a_f)
hist(a)

print(pearson.test(a,   n.classes=floor(3.32*log10(N)+1)))
print(pearson.test(a_f, n.classes=floor(3.32*log10(N)+1)))

for(i in c(3,12,48)){
  a_i <- ksi_i(N, i)
  print(pearson.test(a_i, n.classes=floor(3.32*log10(N)+1)))
  hist(a_i)
}