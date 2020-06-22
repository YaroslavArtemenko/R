polynomial_integral <- function(x) (x^2-1)*(x^2+1)
trigonometric_integral <- function(x) (x+1)*cos(2*x)
infinite_integral <- function(x) ln(x+1)/sqrt(x)

get_integral_limit <- function(n){
  l_lim <- c(0, 0, 10^-3)
  r_lim <- c(1, pi/3, 10^3)
  for(i in 1:3){
    if(n == i){
      bottom_limit <- l_lim[i]
      top_limit <- r_lim[i]
      return(c(bottom_limit, top_limit))
    }
  }
}

integral_counting <- function(n, func){
  cat("Enter the length of random array : ")
  
  N <- scan(n=1)
  integral_data <- get_integral_limit(n)
  
  min_func <- min(func(seq(integral_data[1], integral_data[2], length=N)))
  max_func <- max(func(seq(integral_data[1], integral_data[2],length=N)))
  
  a <- runif(N, min=integral_data[1], max=integral_data[2])
  b <- runif(N, min=min_func, max=max_func)
  
  num_integral_positive <- b >= 0 & b <= func(a)
  num_integral_negative <- b <= 0 & b >= func(a)
  num_integral <- sum(num_integral_positive)-sum(num_integral_negative)
  
  coll_intrgral <- num_integral/N
  
  integral <- coll_intrgral*(max_func-min_func)*integral_data[2]
  cat("\nRecieved solve is : ", integral, "\n\n")
}

while(TRUE){
  for(i in 1:3){
    cat("Enter", i, "to solve", i, "integral : \n")
  }
  
  cat("Enter 0 to end this session\n")
  choice_integral <- scan(n=1)
  
  if(choice_integral==1){
    integral_counting(choice_integral, polynomial_integral)
  }
  else if(choice_integral==2){
    integral_counting(choice_integral, trigonometric_integral)    
  }
  else if(choice_integral==3){
    integral_counting(choice_integral, infinite_integral)
  }
  else if(choice_integral==0){
    break
  }
}