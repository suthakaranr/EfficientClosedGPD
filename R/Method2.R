Method2 = function(x) {
    n = length(x)
    m = 5
    l = c(1:m)
    k_PQ = delta_PQ = NULL
    All_Q = (n - m + l)/(n + 1)
    for (i in 1:length(All_Q)) {
      Q = All_Q[i]
      sort_x = sort(x)
      P = 1 - sqrt(1 - Q)
      n_1 = ceiling((n + 1) * P)
      n_2 = ceiling((n + 1) * Q)
      X_n1 = sort_x[n_1]
      X_n2 = sort_x[n_2]
      k_PQ[i] = log(X_n2/X_n1 - 1)/log(1 - P)
      delta_PQ[i] = ((X_n1)^2)/(2 * X_n1 - X_n2)
    }
    sigma_PQ = k_PQ * delta_PQ
    k2hat0 = median(k_PQ)
    sigma2hat0 = median(sigma_PQ)
    W1 = k2hat0 * max(x)/sigma2hat0
    k2hat = ifelse(W1 < 1, k2hat0, max(k_PQ))
    sigma2hat = ifelse(W1 < 1, sigma2hat0, max(sigma_PQ))
    return(list("sigma "= sigma2hat, "k"= k2hat))
}

#x = runif(25)
#Method2(x)
