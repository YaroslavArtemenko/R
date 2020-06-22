cat("\014")
M=9
NG=20
n=NG/2
N=round(2**n)



s2<-array(N)

for( i  in 1:N)
{
  s2[i]=2*runif(1)+NG*cos(2*M*pi*i/N)*(1+0.1*runif(1))+17*cos(4*M*pi*i/N + runif(1))+3*cos(5*pi*M*i/N)*(runif(1)+NG)
}

s1=s2
A<-array(round(N/2+1))

i=1
A[i]=0
A[i]=(sum(s1)-s1[N])/N

i=round(N/2+1)
A[i]=0
  for (j in 1:(N-1)) {
    A[i]=A[i]+s1[j]*cos(pi*j/N)
  }
  A[i]=A[i]/N

for(i in 2:(round(N/2))) {
  A[i]=0
  for (j in 1:(N-1)) {
    A[i]=A[i]+s1[j]*cos(2*pi*j*i/N)*2/N
  }
}

B<-array(round(N/2+1))
B=0

for(i in 1:(round(N/2+1))) {
  B[i]=0
  for (j in 1:(N-1)) {
    B[i]=B[i]+s1[j]*sin(2*pi*j*i/N)*2/N
  }
}

C<-array(round(N/2+1))
C=sqrt(A**2+B**2)
plot(C,type = "l",col=60)

d1<-array(N)
SumA=0
SumB=0

for (i in 1:N) {
  SumA=0
  SumB=0
  for (j in 1:(round(N/2+1))) {
    SumA=A[j]*cos(2*pi*j*i/N)+SumA
    SumB=B[j]*sin(2*pi*j*i/N)+SumB
  }
  d1[i]=SumA+SumB
}


