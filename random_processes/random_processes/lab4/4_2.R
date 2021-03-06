fundamentalMatrix <- function(T,vector) {
  n = length(T[,1])
  okvector =  findVectors(T)
  
  canonicalMatrix <- canonicalForm(T,okvector)
 
  count = canonicalMatrix$count
  m = n - count;
  Q = matrix(0,ncol = m, nrow=m )
  R = matrix(0,ncol = count, nrow=m )
  
  k_1 = 0
  k_2  = 0
    for (i in (count+1):n){
    k_1 = k_1 + 1;
    k_2  = 1
        for (j in (count+1):n) {
      Q[k_1,k_2] = canonicalMatrix$matrix[i,j]
      k_2 = k_2 + 1
    }
    }
  
  k_1 = 0
  k_2  = 0
  for (i in (count+1):n){
    k_1 = k_1 + 1;
    k_2  = 1
    for (j in 1:count) {
      R[k_1,k_2] = canonicalMatrix$matrix[i,j]
      k_2 = k_2 + 1
    }
  }
  
 
  cat("������������ �������\n")
  print(canonicalMatrix$matrix)
  
  N = solve(diag(m)-Q)
  cat("��������������� �������\n")
  print(N)
  
  cat("����������� ����������\n")
  print(N%*%R)
  cat("������������� ����� ���������� � ������� ��������� \n")
  print(vector3%*%N)
  cat("������� ����� ���������� \n")
  I = matrix(1,ncol = 1,nrow = m)
  print(N%*%I)
  }
canonicalForm <- function(T,okvector) {
    count = 0;
    count_row = 0;
    n = length(T[,1])
    for (i in 1:n) {
      if (okvector[1,i]==1) {
        count_row = count_row + 1;
        T <- swapRow(T,count_row,okvector[2,i])
        count = count + 1
        T <- swapCol(T,count,i)
        if ((i+1)>n){
          break
        }
        for (k in (i+1):n) {
          if (okvector[1,k] == 1 && okvector[2,k]<okvector[2,i])
          {
            okvector[2,k] = okvector[2,k]+1
          }
        }
      }
    }
    my_list <- list(matrix = T, count = count)
    return(my_list)
}
findVectors <- function(T) {
  n = length(T[,1])
  okvector = matrix(FALSE,nrow = 2,ncol = n)
  count_vectical = 0
  for (i in 1:n) 
  {
    for (j in 1:n) 
    {
      if (T[i,j]==1 && okvector[1,j]) 
      {
        flag = FALSE
        break
      }
      if (T[i,j]==1) 
      {
        
        okvector[1,j] = TRUE
        okvector[2,j] = i
      }
    }
  }
  return(okvector)
}

swapCol <- function(T,i,j) {
  a = T
  for (q in i:j) {
    w = T[,j]
    T[,j]  = T[,q]
    T[,q] = w
  }
  return(T)
}

swapRow <- function(T,i,j) {
  b = T
  for (q in i:j) {
    w = T[j,]
    T[j,]  = T[q,]
    T[q,] = w
    }
  
  return(T)
}



n =5
#1,0,0,0,0,0,1,0,0,0,0.4,0,0,0.6,0,0,0,0.4,0,0.6,0,0.6,0,0.4,0
T = matrix(c(0,0,1,0,0,0,0.4,0,0,0.6,0,0,0,0.4,0,0.6,0,0.6,0,0.4,0,1,0,0,0),nrow =n, ncol=n)
vector = c(0.2,0.2,0.3,0.1,0.2)
vector3 = matrix(c(0.2,0.3,0.1),nrow=1, ncol=3)
T = t(T)
print(T)
fundMatrix = fundamentalMatrix(T,vector)
ok=findVectors(T)
print(ok)

