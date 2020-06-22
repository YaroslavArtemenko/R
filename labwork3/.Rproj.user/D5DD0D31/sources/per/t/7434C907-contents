get_state = function(pn, p){
  F = pn[1]
  i = 1
  while(F < p){
    i = i + 1
    F = F + pn[i]
  }
  
  i
}


standartize_matrix = function(matr){
  size = length(matr[, 1])
  I_size = 0
  temp_vector = c()
  for (i in 1:size){
    if(matr[i, i] == 1){
      I_size = I_size + 1
      temp_vector = matr[, I_size]
      matr[, I_size] = matr[, i]
      matr[, i] = temp_vector
      temp_vector = matr[I_size, ]
      matr[I_size, ] = matr[i, ]
      matr[i, ] = temp_vector
    }
  }
  matr
}

get_I_size = function(stan_matr){
  I_size = 0
  size_matr = length(stan_matr[1, ])
  for (i in 1:size_matr){
    if(stan_matr[i, i] == 1){
      I_size = I_size + 1
    }
  }
  I_size
}

eye = function(dimens){
  matr = diag(1, dimens)
  matr
}

n = 5
p01 = c(0, 0, 1, 0, 0)
p02 = c(0.1, 0.2, 0.3, 0.3, 0.1)
m = 100
states1 = c()
states2 = c()
P = matrix(data = c(1/n), nrow = n, ncol = n)
P[3, ] = c(0, 0, 1, 0, 0)
P[5, ] = c(0, 0, 0, 0, 1)
ps1 = runif(m + 1)
ps2 = runif(m + 1)
pn1 = c()
pn2 = c()


P = standartize_matrix(P)
P_size = length(P[, 1])
I_size = get_I_size(P)
I = eye(I_size)
R = P[(I_size + 1):P_size, 1:I_size]
Q = P[(I_size + 1):P_size, (I_size + 1):P_size]
M = solve(eye(P_size - I_size) - Q)
pi_vector1 = p01[(I_size + 1):P_size]
tau_vector1 = pi_vector1%*%M
B = M%*%R
t_absorp1 = sum(tau_vector1)

tau_vector_exp1 = rep(0, P_size - I_size)
t_absorp_exp1 = 0
states1[1] = get_state(p01, ps1[1])
print(states1[1])
for (i in 1:(m - 1)){
  states1[i + 1] = get_state(P[states1[i], ], ps1[i + 1])
  if(states1[i] > I_size){
    tau_vector_exp1[states1[i] - I_size] = tau_vector_exp1[states1[i] - I_size] + 1
  }
}
t_absorp_exp1 = sum(tau_vector_exp1)


pi_vector2 = p02[(I_size + 1):P_size]
tau_vector2 = pi_vector2%*%M
t_absorp2 = sum(tau_vector2)

tau_vector_exp2 = rep(0, P_size - I_size)
t_absorp_exp2 = 0
states2[1] = get_state(p02, ps2[1])
for (i in 1:(m - 1)){
  states2[i + 1] = get_state(P[states2[i], ], ps2[i + 1])
  if(states1[i] > I_size){
    tau_vector_exp2[states1[i] - I_size] = tau_vector_exp2[states1[i] - I_size] + 1
  }
}
t_absorp_exp2 = sum(tau_vector_exp2)

print("Вірогідність поглинання:")
print(B)
print("Точний стан:")
print("Середня тривалість перебування в відповідних станах:")
print(tau_vector1)
print("Середній час поглинання:")
print(t_absorp1)
print("Середня тривалість перебування у відповідних станах (експериментальні):")
print(tau_vector_exp1)
print("Середній час поглинання(експериментальний):")
print(t_absorp_exp1)
print("Нестабільний стан:")
print("Середня тривалість перебування в відповідних станах:")
print(tau_vector2)
print("Середній час поглинання:")
print(t_absorp2)
print("Середня тривалість перебування у відповідних станах (експериментальні):")
print(tau_vector_exp2)
print("Середній час поглинання(експериментальний):")
print(t_absorp_exp2)
