library(nortest)

cat("Enter the lenght of array : ")

N <- scan(n=1)
a <- rnorm(N)

print(ad.test(a))
print(cvm.test(a))
print(lillie.test(a))
print(pearson.test(a))
print(sf.test(a))

hist(a)