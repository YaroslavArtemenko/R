fundamentalMatrix <- function(T,vector,n) {
    W = matrix(0,nrow = n,ncol = n)
    b = matrix(1,nrow = n,ncol = 1)
    for (j in 1:n) {
      for (i in 1:n) {
        if (i != j)
          W[i,j] = 1
          W[i,j] = W[i,j] + T[j,i]
      }
    }
    a  = solve(W)
    print(W)
    s = a%*%b
    s = t(s)
    print("Предельный вектор a для которого выполняется aP = a /n")
    print(s)
    A = matrix(0,ncol = n,nrow = n)
    for (i in 1:n) {
      for (j in 1:n) {
        A[i,j] = s[j]
      }
    }
    print("Матрица финальных вероятностей/n")
    print(A)
    E = diag(1,nrow = n,ncol = n)
    Z = (E - (T - A))
    print("Фундаментальная матрица/n")
    Z = solve(Z)
    print(Z)
    DM = matrix(0,ncol=n,nrow=n);
    DZ = matrix(0,ncol=n,nrow=n);
        for (i in 1:n){
          DM[i,i] = 1/s[i];
          DZ[i,i] = Z[i,i];
        }
    print(DM)
    print(DZ)
    E = matrix(1,ncol=n,nrow=n)
    I = diag(1,nrow=n,ncol=n)
    M = (I-Z+E%*%DM)%*%DZ
    cat("Матрица средних времен достижения/n ")
    print(M)
    cat("Среднее время нахождения в состоянии")
    t = vector%*%M
    print(t)
    cat("Стационарный вектор состояний")
    print(s%*%M)
}

n =5
#0.1, 0.2, 0.3, 0, 0,
#0.2, 0, 0.2, 0, 0,
#0.3, 0.2, 0.2, 0,0,
#0.2,0.4,0.2,0.1,0.1,
#0.3, 0.1, 0.1,0.3, 0.2

#0.7, 0.1, 0, 0.1, 0.1
T = matrix(c(0.1, 0.2, 0.3, 0, 0,
             0.2, 0, 0.2, 0, 0,
             0.3, 0.2, 0.2, 0,0,
             0.2,0.4,0.2,0.1,0.1,
             0.3, 0.1, 0.1,0.3, 0.2),nrow =n, ncol=n)
vector = c(0.7, 0.1, 0, 0.1, 0.1)
T = t(T)
cat("Матрица перехода/n")
print(T)
cat("Матрица перехода в 10 степени /n")
print((T %^% 10))
fundMatrix = fundamentalMatrix(T,vector,n)

