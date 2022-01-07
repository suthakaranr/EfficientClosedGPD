Method1 = function(x){
  n = length(x)
  All_Q = c(0.5, 0.6, 0.75, 0.85, n/(n + 1))
  for (i in 1:length(All_Q)) {
    Q = All_Q[i]
    sort_x = sort(x)
    P = 1 - sqrt(1 - Q)
    n_1 = ceiling((n + 1) * P)
    n_2 = ceiling((n + 1) * Q)
    X_n1 = sort_x[n_1]
    X_n2 = sort_x[n_2]
    k_PQ = log(X_n2/X_n1 - 1)/log(1 - P)
    delta_PQ = ((X_n1)^2)/(2 * X_n1 - X_n2)
  }
  sigma_PQ = k_PQ * delta_PQ
  k1hat0 = median(k_PQ)
  sigma1hat0 = median(sigma_PQ)
  W1 = k1hat0 * max(x)/sigma1hat0
  k1hat = ifelse(W1 < 1, k1hat0, max(k_PQ))
  sigma1hat = ifelse(W1 < 1, sigma1hat0, max(sigma_PQ))
  return(list("sigma "= sigma1hat, "k"= k1hat))
}

#x = runif(25)
#Method1(x)
