N<-2000
z <- runif(n =2000 ,min=0, max=10)
R <- (max(z) - min(z))
k = round(1 + 3.3*log10(N))
dx <- R/k
n<-c()
v<-c()
vt<-c()
for(i in 1:k){
  n[i] <- length(z[z >= min(z) + (i-1)*dx & z <= (min(z) + i*dx)])
  v[i]<-n[i]/1000
  vt[i]<-((v[i]-dx)^2)/dx
}
sum(n)
sum(v)
xi2<-sum(vt)
Xi2<-qchisq(p=0.95, df=7)
if(xi2>Xi2){
  answer<-"Отклоняем гипотезу";
}else{
  answer<-"Нет смысла отклонить гипотезу";
}
hist(z, col="blue")
print(answer)