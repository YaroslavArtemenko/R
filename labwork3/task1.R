get_state = function(pn, p){
  F = pn[1]
  i = 1
  while(F < p){
    i = i + 1
    F = F + pn[i]
  }
  
  i
}

n = 5
p01 = c(0, 0, 1, 0, 0)
p02 = c(0.1, 0.2, 0.3, 0.3, 0.1)
m = 10
states1 = c()
states2 = c()
P = matrix(data = c(1/n), nrow = n, ncol = n)
P[1, ] = c(1, 0, 0, 0, 0)
ps1 = runif(m + 1)
ps2 = runif(m + 1)
pn1 = c()
pn2 = c()

states1[1] = get_state(p01, ps1[1])
for (i in 1:(m - 1)){
  states1[i + 1] = get_state(P[states1[i], ], ps1[i + 1])
}

states2[1] = get_state(p02, ps2[1])
for (i in 1:(m - 1)){
  pn = p02*P
  states2[i + 1] = get_state(P[states2[i], ], ps2[i + 1])
}

print(states1)
print(states2)