library("pracma", lib.loc="E:/RStudio/R-3.5.1/library")
library("stats", lib.loc="E:/RStudio/R-3.5.1/library")
cat("\014")
n=1000

x=linspace(-5, 5, n)

#Вейвлет Хаара
y_haar <- function(x) if((0<=x)&(x<=1/2)) y=1 else if((1/2<=x)&(x<1)) y=-1 else y=0
y_x <- array(n)

for (i in 1:n) {
  y_x[i]=y_haar(x[i])
}

plot(x,y_x,type = "l",col=60,main="Вейвлет Хаара")

#Вейвлет Шенона
sinc <- function(x)sin(pi*x)/(pi*x)
y_shannon <- function(x) sinc(x/2)*cos(3/2*pi*x)
y_x <- array(n)

for (i in 1:n) {
  y_x[i]=y_shannon(x[i])
}
fft(y_x, inverse = TRUE)

plot(x,y_x,type = "l",col=60,main="Вейвлет Шенона")

#Вейвлет Гауса
y_gauss <- function(x) exp(-x**2/2)
y_x <- array(n)

for (i in 1:n) {
  y_x[i]=y_gauss(x[i])
}

plot(x,y_x,type = "l",col=60,main="Вейвлет Гауса")

