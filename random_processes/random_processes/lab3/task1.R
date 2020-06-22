


cat("Enter lenght array")
N <- scan(n=1)
cat("Enter flux density")
L <- scan(n=1)

a <- rpois(N, lambda = L)

hist(a)
print(poisson.test(sum(a),N, L))



