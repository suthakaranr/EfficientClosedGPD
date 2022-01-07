k_PQ = delta_PQ =  NULL
Method15 = function(x){
  n = length(x)
  sort_x = sort(x)
  All_Q = c(0.5, 0.6, 0.75, 0.85, n/(n + 1))
  for (i in 1:length(All_Q)) {
    Q = All_Q[i]
    P = 1 - sqrt(1 - Q)
    n_1 = ceiling((n + 1) * P)
    n_2 = ceiling((n + 1) * Q)
    X_n1 = sort_x[n_1]
    X_n2 = sort_x[n_2]
    k_PQ[i] = log(X_n2/X_n1 - 1)/log(1 - P)
    delta_PQ[i] = ((X_n1)^2)/(2 * X_n1 - X_n2)
  }
  sigma_PQ = k_PQ * delta_PQ
  k1hat0 = median(k_PQ)
  sigma1hat0 = median(sigma_PQ)
  W1 = k1hat0 * max(x)/sigma1hat0
  k1hat = ifelse(W1 < 1, k1hat0, max(k_PQ))
  sigma1hat = ifelse(W1 < 1, sigma1hat0, max(sigma_PQ))
  return(c(sigma1hat, k1hat))
}



fnn = function(para, x){
  n = length(x)
  x = sort(x)
  c = numeric(n - 1)
  d = numeric(n - 1)
  for (i in 2:n){
    c[i - 1] = pgpd2(x[i], para[1], para[2]) 
    d[i - 1] = pgpd2(x[i-1], para[1], para[2]) 
  }
  s = sum(log(c - d))
  return(-s)
}

methodMPOS = function(x) {
      est = Method15(x)
      opt2 = optim(c(est), fnn, x = x)
      sigma1hat = opt2$par[1]
      k1hat = opt2$par[2]
      return(list("sigma"= sigma1hat, "k"= k1hat))
}

#methodMPOS(25, 1, 2)
#methodMPOS(x)
