fa <- function(x,y) {
  return(x^2+y^2)
}
fb <- function(x,y){
  return (16*x^2 + 9*y^2)
}
fc <- function(x,y){
  return (9*x^2 + 16*y^2)
}

 integral<- function(N){
   ax <- runif(N, min =(-2), max =2)
   ay <- runif(N, min = (-2), max =2)
   M =  0
   for (i in 1:N){
     if ((fa(ax[i], ay[i]) <= 2) && (fb(ax[i], ay[i]) <=25) && (fc(ax[i], ay[i]) <= 25)){
       M = M + 1 
     } 
   }
   integral  <- 9*M/N
   cat( "integral = ", integral, "\n" )
 }
   
  cat ("Enter the lenght of iterations : ")
  N <- scan(n=1)
  integral(N)