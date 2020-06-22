N = 100
NUMBER = 1
k = 5
m = 4
FTeora = 3.26;
FTeorb = 3.49;
FTeor1 = 1.75;
mas <- array(0, dim=c(m, k, N)) 
ar  <- array(0, c(m,k))
sum = 0;
koefData = c(1, 3.5, 3.8, 1.4, 2,
			1, 2.5, 2.8, 2.4, 3,
			1, 1.5, 1.8, 3.4, 4,
			1, 0.5, 0.8, 4.4, 5)
koef<-matrix(koefData, nrow=4, ncol=5,byrow = TRUE)
array <- matrix(0,nrow = m, ncol = k)

for ( i  in 1:m) {
  for ( j   in 1:k) {
    sum = 0;
    for (q in 1 : N ) {
      s <- NUMBER * koef[i,j] + runif(1)
      mas[i,j,q] = s
      sum = sum +  mas[i,j,q];
    }
    array[i,j] = sum / N;
  }
}


sumOfMasPow  <- function() {
  return (sum(mas*mas))
}

sumOfXiPow <- function() {
  sum = sum(array)
  return (sum * sum )
}

sumOfXj  <- function() { 
  sum = 0;
  for (i  in 1 : m ) {
    localSum = 0;
    for (j  in 1 : k ) {
      localSum = localSum +  array[i,j];
    }
    sum = sum +  (localSum * localSum);
  }
  return (sum);
}

sumOfXi <- function() {
  sum = 0;
  for (j  in 1 : k ) {
    localSum = 0
    for (i   in 1 : m ) {
      localSum  = localSum +  array[i,j];
    }
    sum =  sum  + (localSum * localSum);
  }
  return (sum);
}

sumArrayAvg <- function() {
  sum = 0
  for ( i  in 1 : m ) {
    for (j  in 1 : k ) {
      sum = sum + (array[i,j] * array[i,j]);
    }
  }
  return (sum);
}

 Q = sumArrayAvg();
 Q2 = sumOfXi() / m;
 Q3 = sumOfXj() / k;
 Q4 = sumOfXiPow() / (m * k);
 Q5 = sumOfMasPow();
 S0 = (Q + Q4 - Q2 - Q3) / ((k - 1) * (m - 1));
 Sa = (Q2 - Q4) / (k - 1);
 Sb = (Q3 - Q4) / (m - 1);
 Sab = (Q5 - N * Q) / (m * k * (N - 1));
 cat("Q1 = " , Q, "\n")
 cat("Q2 = " , Q2,"\n")
 cat("Q3 = " , Q3,"\n")
 cat("Q4 = " , Q4,"\n")
 cat("Q5 = " , Q5,"\n")
 cat("S0^2 = " , S0,"\n")
 cat("Sa^2 = " , Sa,"\n")
 cat("Sb^2 = " , Sb,"\n")
 cat("Sab^2 = " , Sab,"\n");
 cat("Sa^2 / S0^2 =    " , Sa / S0 ,
                      "\n  Sb^2 / S0^2  = " , Sb / S0 ,
                      "\n  N*S0^2/Sab^2 = ", N * S0 / Sab, "\n")

cat("Fa = ", FTeora, "\n")
cat("Fb = ", FTeorb, "\n")
cat("Fab = ", FTeor1, "\n")
 if (Sa/S0 > FTeora) {
   cat("A is good criteria\n")
 } else {cat("A is bad criteria\n") }
   
 
 if (Sb / S0 > FTeorb) {
   cat("B is good criteria\n")
 } else
   {cat("B is bad criteria\n")}
 if (N * S0 / Sab > FTeor1) {
   cat("Both criterias are good\n")
 } else
   {cat("Both criterias are bad\n")}
 

 