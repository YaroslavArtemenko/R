library(beepr)
cat("\014")

M=9
NG=20
n=NG/2
N=2**round(n)

s1<-array(N)

for( i  in 1:N)
{
  s1[i]=2*runif(1)+NG*cos(2*M*pi*i/N)*(1+0.1*runif(1))+17*cos(4*pi*M*i/N+runif(1))+3*cos(7*pi*M*i/N)*(runif(1)+NG)
}

Ml=N

#Вейвлет Хаара
y_haar_func <- function(x) if((0<=x)&(x<1/2)) y=1 else if((1/2<=x)&(x<1)) y=-1 else y=0

f2_haar <- function(j,k,x) 2^(j/2)*y_haar_func(2^j*x-k)

#Вейвлет Шенона
sinc <- function(x) sin(pi*x)/(pi*x)
y_shannon_func <- function(x) sinc(x/2)*cos(3/2*pi*x)

f2_shannon <- function(j,k,x) 2^(j/2)*y_shannon_func(2^j*x-k)


#Вейвлет Гауса
y_gauss_func <- function(x) -x*exp(-(x)^(2)/2)

f2_gauss <- function(j,k,x) 2^(j/2)*y_gauss_func(2^j*x-k)

W_haar <- array(0,c(M,Ml))
W_shannon <- array(0,c(M,Ml))
W_gauss <- array(0,c(M,Ml))

for (l in 1:M) {
  for (j in 1:Ml) {
    summ_h = 0
    summ_sh = 0
    summ_g = 0
    for (i in 1:(N-1)) {
      summ_h = s1[i]*f2_haar(l-1,j-1,i-1)+summ_h
      summ_sh = s1[i]*f2_shannon(l-1,j-1,i-1)+summ_sh
      summ_g = s1[i]*f2_gauss(l-1,j-1,i-1)+summ_g
    }
    W_haar[l,j] = summ_h
    W_shannon[l,j] = summ_sh
    W_gauss[l,j] = summ_g
  }
}

d_haar<-array(N)
d_shannon<-array(N)
d_gauss<-array(N)

for (i in 1:N) {
  summ_h = 0
  summ_sh = 0
  summ_g = 0
  for (l in 1:M) {
    for (j in 1:Ml) {
      summ_h = W_haar[l,j]*f2_haar(l-1,j-1,i-1)/2^(2*(l-1))+summ_h
      summ_sh = W_shannon[l,j]*f2_shannon(l-1,j-1,i-1)/2^(2*(l-1))+summ_sh
      summ_g = W_gauss[l,j]*f2_gauss(l-1,j-1,i-1)/2^(2*(l-1))+summ_g
    }
  }
  d_haar[i] = summ_h
  d_shannon[i] = summ_sh
  d_gauss[i] = summ_g
}

plot(s1-d_haar,type = "l",col=60,main="Вейвлет Xаара")
#plot(s1-d_shannon,type = "l",col=60,main="Вейвлет Шенона")
plot(s1-8*d_gauss,type = "l",col=60,main="Вейвлет Гауса")

if(NG>=15) {beepr::beep(10)}
