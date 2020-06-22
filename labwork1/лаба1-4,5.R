library(nortest)
N=1000
n=c(3,12,48)
ksi_1 <- function(N){
  a1 <- runif(N)
  a2 <- runif(N)
  ksi <- c()
  
  for(i in 1:N){
    ksi[i] = sqrt(-2*log(a1[i]))*cos(2*pi*a2[i])
  }
  return(ksi)
}
ksi_2 <- function(N){
  a1 <- runif(N)
  a2 <- runif(N)
  ksi <- c()
  for(i in 1:N){
    ksi[i] = sqrt(-2*log(a1[i]))*sin(2*pi*a2[i])
  }
  return(ksi)
}
N=1000
n=c(3,12,48)
ksi_3 = function(N,n){
  ksi= c()
  for(k in length(n)){
    for(i in 1:N){
      ksi[i]=0;
      a=runif(200)
      for(j in 1:n[k]){
        ksi[i]=ksi[i]+sqrt(12/n[k])*(a[j]-0.5)
      }}}
  ksi
}
shapiro.test(ksi_1(N))
hist(ksi_1(N), col = "lightgreen")
shapiro.test(ksi_2(N))
hist(ksi_2(N), col = "darkred")
for(i in 1:3){
  print(shapiro.test(ksi_3(N,n[i])))
  hist(ksi_3(N,n[i]), col = "lightyellow")
}