library(ggplot2)
library(reshape2)




cat("enter the dimension of the matrix")
N = scan(n = 1)

cat("enter the number of implementations")
m = scan(n = 1)

cat("enter the number of steps")
k = scan(n = 1)



T = matrix(0,N,N)

for (i in 1:N)
{
  cat("enter the string")
  T[i,] = scan(n = N)
}

cat("enter the vector of initial states")
v = scan(n = N)



  TT = T
  n = length(T[,1])
  y = matrix(0,ncol = k+1, nrow = m+1)

 
  for(l in 1:m+1) {
      r = runif(1)
  step_value  = 0;
  node = 0;
  
    for (j in 1:n) 
  {
    step_value = step_value + v[j]
    if (step_value >= r)  {
      node  = j
      break
    }
  }
  
  y[l,1] = node 
  
  for (i in 1:k) 
  {
    r = runif(1)
    step_value  = 0;
    for (j in 1:n) 
    {
      value = TT[node,j]
      step_value = step_value + value
      if (step_value >= r)  {
        node  = j
        break
      }
    }
    y[l,i + 1] = node
  }
  plot(y[l,])

  }
