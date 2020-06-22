n<-0
for (i in 1:100)
{
  x<-runif(rnorm(1000), 0, 1);
  n[i]<-max(x);
}
hist(n, col="blue")