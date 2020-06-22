n<-0
for (i in 1:100)
{
  x<-rnorm(1000);
  n[i]<-max(x);
}
hist(n, col="blue")