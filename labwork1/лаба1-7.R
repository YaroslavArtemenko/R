#n=1/2
library(ggplot2)
library(reshape2)
N = 100
t=10
ksi_a = function(t){
  eta1 = c(); eta2 = c(); ksi=0
  for(i in 1:1000){
    eta1=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2))))
    eta2=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2))))
    ksi=ksi+cos(i*pi*t)*eta1+sin(i*pi*t)*eta2
  }
  ksi
}
a=matrix(data = NA,nrow = N,ncol = t);qa=c()
for (j in 1:t) {
  for(i in 1:N){
    a[i,j]=ksi_a(j)
  }
  plot(a[,j],xlim = c(1,N),type = "l")
  par(new= TRUE)
  qa[j]=mean(a[,j])
}
plot(qa, col="darkblue")

me1=mean(a)
print(mean(a))
D1=0
for (j in 1:t){
  for(i in 1:N){
    D1=D1 + sqrt(((a[i,j]-me)^2)/N)
  }
}
print(D1)

cor(a)
library(Hmisc)
b=rcorr(a)
cor(a,method="pearson")


#-------------------------------------------------
#n=1
library(ggplot2)
library(reshape2)
N = 100
t=10
ksi_a = function(t){
  eta1 = c(); eta2 = c(); ksi=0
  for(i in 1:1000){
    eta1=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^2)))
    eta2=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^2)))
    ksi=ksi+cos(i*pi*t)*eta1+sin(i*pi*t)*eta2
  }
  ksi
}
a=matrix(data = NA,nrow = N,ncol = t);qa=c()
for (j in 1:t) {
  for(i in 1:N){
    a[i,j]=ksi_a(j)
  }
  plot(a[,j],xlim = c(1,N),type = "l")
  par(new= TRUE)
  qa[j]=mean(a[,j])
}
plot(qa, col="darkblue")

me2=mean(a)
print(mean(a))
D2=0
for (j in 1:t){
  for(i in 1:N){
    D2=D2 + sqrt(((a[i,j]-me)^2)/N)
  }
}
print(D2)

cor(a)
library(Hmisc)
b=rcorr(a)
cor(a,method="pearson")


#---------------------------------------------------
#n=2
library(ggplot2)
library(reshape2)
N = 100
t=10
ksi_a = function(t){
  eta1 = c(); eta2 = c(); ksi=0
  for(i in 1:1000){
    eta1=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^4)))
    eta2=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^4)))
    ksi=ksi+cos(i*pi*t)*eta1+sin(i*pi*t)*eta2
  }
  ksi
}
a=matrix(data = NA,nrow = N,ncol = t);qa=c()
for (j in 1:t) {
  for(i in 1:N){
    a[i,j]=ksi_a(j)
  }
  plot(a[,j],xlim = c(1,N),type = "l")
  par(new= TRUE)
  qa[j]=mean(a[,j])
}
plot(qa, col="darkblue")

me3=mean(a)
print(mean(a))
D3=0
for (j in 1:t){
  for(i in 1:N){
    D3=D3 + sqrt(((a[i,j]-me)^2)/N)
  }
}
print(D3)

cor(a)
library(Hmisc)
b=rcorr(a)
cor(a,method="pearson")


#----------------------------------------------------
#n=3
library(ggplot2)
library(reshape2)
N = 100
t=10
ksi_a = function(t){
  eta1 = c(); eta2 = c(); ksi=0
  for(i in 1:1000){
    eta1=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^6)))
    eta2=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^6)))
    ksi=ksi+cos(i*pi*t)*eta1+sin(i*pi*t)*eta2
  }
  ksi
}
a=matrix(data = NA,nrow = N,ncol = t);qa=c()
for (j in 1:t) {
  for(i in 1:N){
    a[i,j]=ksi_a(j)
  }
  plot(a[,j],xlim = c(1,N),type = "l")
  par(new= TRUE)
  qa[j]=mean(a[,j])
}
plot(qa, col="darkblue")

me4=mean(a)
print(mean(a))
D4=0
for (j in 1:t){
  for(i in 1:N){
    D4=D4 + sqrt(((a[i,j]-me)^2)/N)
  }
}
print(D4)

cor(a)
library(Hmisc)
b=rcorr(a)
cor(a,method="pearson")


#------------------------------------------------
#n=4


library(ggplot2)
library(reshape2)
N = 100
t=10
ksi_a = function(t){
  eta1 = c(); eta2 = c(); ksi=0
  for(i in 1:1000){
    eta1=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^8)))
    eta2=rnorm(1,mean = 0, sd = (1/( (1 + pi * i^2)^8)))
    ksi=ksi+cos(i*pi*t)*eta1+sin(i*pi*t)*eta2
  }
  ksi
}
a=matrix(data = NA,nrow = N,ncol = t);qa=c()
for (j in 1:t) {
  for(i in 1:N){
    a[i,j]=ksi_a(j)
  }
  plot(a[,j],xlim = c(1,N),type = "l")
  par(new= TRUE)
  qa[j]=mean(a[,j])
}
plot(qa, col="darkblue")

me5=mean(a)
print(mean(a))
D5=0
for (j in 1:t){
  for(i in 1:N){
    D5=D5 + sqrt(((a[i,j]-me)^2)/N)
  }
}
print(D5)

cor(a)
library(Hmisc)
b=rcorr(a)
cor(a,method="pearson")