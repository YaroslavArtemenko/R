cat("Enter the length random of array : ")
N <- scan(n=1)
cat("Enter the number of iterations : ")
M <- scan(n=1)

h <- array(0, M)
for (i in 1:M){
  b <- runif(N)
  h[i] = max(b)
}

hist(h)