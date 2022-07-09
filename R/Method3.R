Method13 = function(x,n){
  k_PQ = delta_PQ = NULL
  All_Q = c(0.5, 0.6, 0.75, 0.85, n/(n+1))
  for(i in 1:length(All_Q)){
    sort_x = sort(x)
    Q = All_Q[i]
    P = 1 - sqrt(1-Q)
    n_1 = ceiling((n+1)*P)
    n_2 = ceiling((n+1)*Q)
    X_n1 = sort_x[n_1]
    X_n2 = sort_x[n_2]
    k_PQ[i] = log(X_n2/X_n1 - 1)/log(1 - P)
    delta_PQ[i] = ((X_n1)^2)/(2*X_n1 - X_n2)
  }
  sigma_PQ = k_PQ * delta_PQ
  k13hat0 = median(k_PQ)
  sigma13hat0 = median(sigma_PQ)
  W1 = k13hat0 * max(x)/sigma13hat0
  k13hat = ifelse(W1<1, k13hat0, max(k_PQ))
  sigma13hat = ifelse(W1<1, sigma13hat0, max(sigma_PQ))
  return(c(k13hat, sigma13hat))
}

k_PQ = delta_PQ = NULL
Method23 = function(x, n){
  m = 5
  l = c(1:m)
  All_Q = (n - m + l)/(n+1)
  for(i in 1:length(All_Q)){
    sort_x = sort(x)
    Q = All_Q[i]
    P = 1 - sqrt(1-Q)
    n_1 = ceiling((n+1)*P)
    n_2 = ceiling((n+1)*Q)
    X_n1 = sort_x[n_1]
    X_n2 = sort_x[n_2]
    k_PQ[i] = log(X_n2/X_n1 - 1)/log(1 - P)
    delta_PQ[i] = ((X_n1)^2)/(2*X_n1 - X_n2)
    sigma_PQ = k_PQ * delta_PQ
  }
  sigma_PQ = k_PQ * delta_PQ
  k23hat0 = median(k_PQ)
  sigma23hat0 = median(sigma_PQ)
  W1 = k23hat0 * max(x)/sigma23hat0
  k23hat = ifelse(W1<1, k23hat0, max(k_PQ))
  sigma23hat = ifelse(W1<1, sigma23hat0, max(sigma_PQ))
  return(c(k23hat, sigma23hat))
}

Method3 = function(x){
    n = length(x)
    est1 = Method13(x, n)
    est2 = Method23(x, n)
    k3hat = (est1[1] + est2[1])/2
    sigma3hatzero = ifelse(est1[1] <= 1/4, est1[2], (est1[2] + est2[2])/2)
    w3 = k3hat*max(x)/sigma3hatzero
    sigma3hat = ifelse(w3 < 1, sigma3hatzero, (est1[2] + est2[2])/2)
    return(list("sigma "= sigma3hat, "k"= k3hat))
}

#x = runif(25)
#Method3(x)
