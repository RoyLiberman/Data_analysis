norm_vec <- function(x) sqrt(sum(x^2))

df = function(X,y) {
  n <- length(y)
  p <- ncol(X)
  return(n-p-1)
}

coefficients = function(X, y){
  X_trans <-  t(A)
  beta_hat <- solve(X_trans %*% X) %*% X_trans %*% y
  return(beta_hat)
}

fit = function(X,y) {
  beta_hat <- coefficients(X, y)
  return(X %*% beta_hat)
}

cor = function(X, y) {
  return(cov(X,y) / (sd(X) * sd(y)))
}

residuals = function(X, y) {
  return(y - fit(X,y))
}

stud.residuals = function(X,y) {
  e <- residuals(X,y)
  return((e - mean(e)) / sd(e))
}

erreta = function(X,y) {
  SST <- sum((y - mean(y))**2)
  SSE <- sum((y - fit(X,y))**2)
  SSR <-  SST - SSE
  return(c(SST, SSR, SSE))
}


s2 = function(X, y) {
  eps_hat <- residuals(X,y)
  norm_eps_hat <- norm_vec(eps_hat)
  return((norm_eps_hat^2) / (df(X,y)))
}

beta.hat.cov <- function(X,y) {
  return(cov(coefficients(X,y)))
}

x.vars <- function(X,y) {
  
}