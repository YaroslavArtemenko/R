cat("Enter the length of random array : ")
N <- scan(n=1)
a <- runif(N)

num_interval <- floor(3.32*log10(N)+1)
list_interval <- seq(0,1, length=num_interval)
list_times_interval <- array(0, num_interval-1)

for (i in a){
  for (j in 1:(num_interval-1)){
    if ( list_interval[j] < i & i < list_interval[j+1]){
      list_times_interval[j] = list_times_interval[j] + 1
    }
  }
}

hist(a)
list_times_interval = list_times_interval/N

list_must_interval <- array(1/num_interval, num_interval-1)
xi <- sum(((list_must_interval-list_times_interval)^2)/list_must_interval)

cat("\nValue of xi^2 : ", xi, "\n")
cat("Distribution is uniform : ",qchisq(p = 0.95, df = length(list_times_interval)-3), "\n")
cat("Probability is 0.95\nDf : ", length(list_times_interval)-1,"\n\n")