prodd <- function(x) {
  sum = 0
  for (i in 1: length(x)) {
    sum = sum + x[i]*x[i]
  }
  return (sum)
}
N = 1000
GTEOR = 0.33
k = 5
variant = 1
FTEOR = 2.37;
cat("f  = ", FTEOR, "\n")
alfa = 0.95;
koef = c(1, 0.5,0.8,1.4,2)
S = c(0,0,0,0,0)
mas = matrix(0, nrow = 5, ncol = N)
for (i in 1:5) {
  for (j in 1:N) {
   mas[i,j] = variant*koef[i] + runif(1)
  }
}
maxS = 0
for (i in 1:5) {
  S[i] = var(mas[i,])
  maxS = max(S[i], maxS);
}

G = maxS / (S[5] + S[1] + S[2] + S[3] + S[4]);
cat("g = ", G, "\n");

if (GTEOR < G) {
  cat("END")
} else {
  sum1 = 0
  sum2 = 0
  sum3 = sum(mas)**2
  for (i in 1:k) {
    sum1 = sum1 + prodd(mas[i,])
    sum2 = sum2 + sum(mas[i,])**2
  }
  sum2 = sum2 / N
  S0_2 = 1/(k*(N-1))*(sum1 - sum2)
  S2 = 1/(k*N - 1) * (sum1 - 1/(k*N)*sum3)
  
  x__ = 0
  
  for (i in 1:k) {
    x__ = x__ + mean(mas[i,])
  }
  x__ = x__/k
  sum4 = 0
  for (i in 1:k) {
    sum4 = sum4 + (mean(mas[i,]) - x__)**2
  } 
  SA_2 = N/(k-1)*sum4
  cat("SA^2  = " , SA_2, "\n")
  cat("S0^2 = " , S0_2, "\n");
  cat("SA^2/S0^2 = " , SA_2 / S0_2, "\n");
  if (SA_2 / S0_2 > FTEOR) cat("A is good criteria")
  else cat("A is bad criteria");
}



