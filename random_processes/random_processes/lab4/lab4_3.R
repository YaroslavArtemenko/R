library(expm) 

chainnumber <- function(v,r) 
{
  sum = 0
  for (i in 1:(length(v))) {
    sum = sum + v[i]
    if (sum >= r) return(i)
  }
}


simulation <- function(N,k,T,V) {
  n = length(T[1,])
  a = matrix(0,ncol = n, nrow = 1)
  b = matrix(FALSE,ncol = n, nrow = 1)
  c = matrix(FALSE,ncol = n, nrow = 1)
  
  #Ќашли все поглощающие цепи 
  for (i in 1:n) {
    for (j in 1:n) {
      if (T[i,j] == 1) {
        b[i] = TRUE
        break()
      }
    }
  }
  #ћоделируем цепи
  for (i in 1:N) {
    rnd  = runif(1)
    s = chainnumber(V,rnd)
          for (j in 1:k) {
          
            rnd  = runif(1)
            s = chainnumber(T[s,],rnd)
            if (b[s]==TRUE) 
                break
          }
    a[s] = a[s] + 1
  }
  
  for (q in 1:n) {
    if (b[q] != TRUE && V[q] != 0)  {
  for (i in 1:N) {
    rnd  = runif(1)
    s = q
    for (j in 1:k) {
       c[q] = c[q] + 1
       rnd  = runif(1)
       s = chainnumber(T[s,],rnd)
       if (b[s] == TRUE) 
        break
       }
      }
    }
  }
  
  print("¬еро€тность оказатьс€ в вершине через к шагов/n")
  print(a/N)
  print(c/N)
  }
#v = веро€тности вершины 
#r = веро€тность перехода которую сгенерировали 
#¬ернет в какую вершину попапли 


n = 5;
N = 40000;
k = 50;
T = matrix(c(1,0,0,0,0,0,1,0,0,0,0.4,0,0,0.6,0,0,0,0.4,0,0.6,0,0.6,0,0.4,0),nrow = n, ncol = n)
vector = c(0.3,0.2,0.2,0.2,0.1)
T = t(T)
print(T)
simulation(N,k,T,vector)
print(vector %*% (T %^% k))


