rgpd2 <- function(n, sigma = 1, k){
  u <- runif(n,0,1)
  r <- qgpd2(u, sigma, k)
  err <- 0
  if (n<=0){
    err <- 1
    cat('Please give a strict positive n!')}
  if (err==0){return(sort(r))}
}

qgpd2 <- function(p, sigma = 1, k){
  n <- length(p)
  q <- 1:n*NA
  for (i in 1:n){
    if (p[i] <= 0){q[i] <- -Inf}
    if (p[i] == 1){q[i] <- max(p)}
    if (p[i] >  1){q[i] <- Inf}
    if (is.na(q[i]) == 1){
      if (k!=0){q[i] <- (1 - (1-p[i])^(k))/k}
      if (k==0){q[i] <- -log(1-p[i])}}
  }
  return(q * sigma)
}


pgpd2 <- function(q, sigma = 1, k){
  n <- length(q)
  p <- 1:n*NA
  if (k>0){
    II <- (1:n)*(q>=0)*(q< sigma/k)
    p[II] <- 1-(1 - k*q[II]/sigma)^(1/k)}
  II <- (1:n)*(q>=0)
  if (k==0){p[II] <- 1-exp(-q[II]/sigma)}
  if (k<0){p[II] <- 1-(1 - k*q[II]/sigma)^(1/k)}
  return(p)
}