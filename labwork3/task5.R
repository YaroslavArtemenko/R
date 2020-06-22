get_state = function(pn, p){
  F = pn[1]
  i = 1
  while(F < p){
    i = i + 1
    F = F + pn[i]
  }
  
  i
}

matr_pow = function(matr, pow){
  for (i in 2:pow){
    matr = round(matr%*%matr, 8)
  }
  matr
}

eye = function(dimens){
  matr = diag(1, dimens)
  matr
}

get_V = function(P, m){
  P_size = length(P[1, ])
  V = matrix(data = c(0), nrow = P_size, ncol = P_size)
  P_pow = eye(P_size)
  for (i in 1:(m - 1)){
    P_pow = P_pow%*%P
    V = V + P_pow
  }
  V
}

get_M = function(Z, f){
  size = length(f)
  I = eye(size)
  E = matrix(data = c(1), nrow = size, ncol = size)
  DZ = diag(diag(Z), size)
  DM = diag(1/f, size)
  M = (I - Z + E%*%DZ)%*%DM
  M
}

n = 5
p01 = c(0, 0, 1, 0, 0)
p02 = c(0.1, 0.2, 0.3, 0.3, 0.1)
m = 100
states1 = c()
states2 = c()
P = matrix(data = c(1/n), nrow = n, ncol = n)
ps1 = runif(m + 1)
ps2 = runif(m + 1)
pn1 = c()
pn2 = c()

P_size = length(P[1, ])
F = matr_pow(P, 100)
f = F[1, ]
Z = solve(eye(P_size) - (P - F))
V = get_V(P, m)
M = get_M(Z, f)
tn1 = p01%*%V
t1 = p01%*%M

states1[1] = get_state(p01, ps1[1])
tn_exp1 = rep(0, P_size)
t_exp1 = rep(0, P_size)
for (i in 1:(m - 1)){
  states1[i + 1] = get_state(P[states1[i], ], ps1[i + 1])
  tn_exp1[states1[i]] = tn_exp1[states1[i]] + 1
  if(t_exp1[states1[i]] == 0){
    t_exp1[states1[i]] = i
  }
}

tn2 = p02%*%V
t2 = p02%*%M

states2[1] = get_state(p02, ps2[1])
tn_exp2 = rep(0, P_size)
t_exp2 = rep(0, P_size)
for (i in 1:(m - 1)){
  pn = p02*P
  states2[i + 1] = get_state(P[states2[i], ], ps2[i + 1])
  tn_exp2[states2[i]] = tn_exp2[states2[i]] + 1
  if(t_exp2[states2[i]] == 0){
    t_exp2[states2[i]] = i
  }
}

print("Фінальна матриця F:")
print(F)
print("Z:")
print(Z)
print("Середня тривалість переміщення між відповідними станами:")
print(V)
print("Середні моменти першого трансферу між відповідними станами:")
print(M)

print("Точний стан:")
print("Середня тривалість перебування в відповідних станах:")
print(tn1)
print("Середні моменти першого прибуття в відповідні стани:")
print(t1)
print("Середня тривалість перебування в відповідних станах (експериментальна):")
print(tn_exp1)
print("Середні моменти першого прибуття в відповідні стани (експериментальні):")
print(t_exp1)


print("Нестабільний стан:")
print("Середня тривалість перебування в відповідних станах:")
print(tn2)
print("Середні моменти першого прибуття в відповідні стани:")
print(t2)
print("Середня тривалість перебування в відповідних станах (експериментальна):")
print(tn_exp2)
print("Середні моменти першого прибуття в відповідні стани (експериментальні):")
print(t_exp2)