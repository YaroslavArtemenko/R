#1
N = 100000;
X = runif(N, 0, 1);
Y = runif(N, 0, 14);
ctr = 0;
for (i in 1:N) {
  if (Y[i] <= (X[i]^7+X[i]^5+X[i]^3)){
    ctr = ctr + 1;
  }
}
result1<-ctr/N*14*1
print(result1)

#f <- function(x) (x^7+x^5+x^3)
#n <- 1e6
#x <- runif(n)
#I <- sum(f(x))/n
#integrate(f,0,1)
#Очікуваний результат: 0.5416667 with absolute error < 6e-15

#--------------------------------------------
#2
N = 100000;
X1 = runif(N, 0, pi / 3);
X2 = runif(N, pi / 3, 2 * pi / 3);
X3 = runif(N, 2 * pi / 3, pi);
Y1 = runif(N, 0, 2);
Y2 = runif(N, -2, 0);
Y3 = runif(N, 0, 2);
ctr1 = 0;
ctr2 = 0;
for (i in 1:N) {
  if (Y1[i] <= 2 * sin(X1[i] * 3)) {
    ctr1 = ctr1 + 1;
  }
  if (Y2[i] >= 2 * sin(X2[i] * 3)) {
    ctr2 = ctr2 + 1;
  }
  
}
square1 = ctr1 / N * 2 * pi / 3 * 2;
square2 = ctr2 / N * pi / 3 * 2;
print(square1);
print(square2);
result2<-square1-square2
print(result2)

# f <- function(x) (2*(sin(3*x)))
# n <- 1e6
# x <- runif(n)
# I <- sum(f(x))/n
# integrate(f,0,pi)
# Очікуваний результат: 1.333333 with absolute error < 1.2e-12

#--------------------------------------------

#3
#1varaiat
N = 100000;
u = runif(N, 0, 1);
X = runif(N, 0, 1);
Y = runif(N, 0, 1);
U = runif(N, 0, 1);
ctr4 = 0;
ctr5 = 0;

for (i in 1:N) {
 if (Y[i] <= 1/((1+x[i]) *X[i]^(1/2))) {
   ctr4 = ctr4 + 1;
  }
}
for (i in 1:N) {
 if (U[i] <= 1/((1+(1/u[i])* u[i]^2*u[i]^(-1/2)))) {
   ctr5 = ctr5 + 1;
 }
}
square3 = ctr4 / N*10;
square4 = ctr5 / N*10;
result3 = (square3 + square4)/2
print(result3)

#2variant
N = 1000
f = function(x) { 1/((x+1)*x^(1/2)) }
b = 1
a = 0
x = runif(N, 0, 1)
sum = 0
for(i in 1:N){
  sum = sum + f(x[i])
}
I=2 * sum / N
I

# 
# f <- function(x) (1/((x+1)*sqrt(x)))
# n <- 1e6
# x <- runif(n)
# I <- sum(f(x))/n
# integrate(f,0,Inf)
# Очікуваний результат: 3.141593 with absolute error < 2.7e-05