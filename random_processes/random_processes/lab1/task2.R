library(exptest)
cat("Enter the length of random array : ")
N <- scan(n=1)
a <- rexp(N)

hist(a)


print(frozini.exp.test(a))
print(kochar.exp.test(a))
print(pietra.exp.test(a))