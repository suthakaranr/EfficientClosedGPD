k_PQ = delta_PQ = NULL
MethodQM = function(x) {
    n = length(x)
    All_Q = c(0.5, 0.6, 0.75, 0.85, n/(n + 1))
    for (i in 1:length(All_Q)) {
      sort_x = sort(x)
      Q = All_Q[i]
      P = 1 - (1 - Q)^(1/3)
      n_1 = ceiling((n + 1) * P)
      n_2 = ceiling((n + 1) * Q)
      X_n1 = sort_x[n_1]
      X_n2 = sort_x[n_2]
      delta_PQ[i] = (-3*X_n1^(2) - sqrt(4*X_n1^(3)*X_n2 - 3*X_n1^(4)))/(2*(X_n2 - 3*X_n1))
      delta = (-3*X_n1^(2) - sqrt(4*X_n1^(3)*X_n2 - 3*X_n1^(4)))/(2*(X_n2 - 3*X_n1))
      k_PQ[i] = log(1 - X_n1/delta)/log(1-P)
    }
    sigma_PQ = k_PQ * delta_PQ
    k4hat0 = median(k_PQ)
    sigma4hat0 = median(sigma_PQ)
    W4 = k4hat0 * max(x)/sigma4hat0
    k4hat = ifelse(W4 < 1, k4hat0, max(k_PQ))
    sigma4hat = ifelse(W4 < 1, sigma4hat0, max(sigma_PQ))
    return(list("sigma"= sigma4hat, "k"= k4hat))
}

#x = runif(25)
#MethodQM(x)
