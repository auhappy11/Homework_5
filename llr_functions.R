#initial

llr = function(x, y, z, omega) {
  fits = sapply(z, compute_f_hat, x, y, omega)
  return(fits)
}


compute_f_hat = function(z, x, y, omega) {
  Wz = diag(make_weight_matrix(z, x, omega)) #changed so that it is vector of weights
  X = make_predictor_matrix(x)
  #used apply function in place of "Wz %*%...."
  f_hat = c(1, z) %*% solve(t(X) %*% apply(X, 2, function(Xcol){Wz * Xcol})) %*% t(X) %*% apply(as.matrix(y), 2, function(Xcol) {Wz*Xcol})
  return(f_hat)
}

make_weight_matrix = function(z, x, omega) {
  x_1 = abs(x - z) / omega
  x_r = sapply(x_1, W)
  weight_matrix = diag(x_r)
  return(weight_matrix)
}

W = function(r) {
  if (abs(x_1) < 1) {
    return((1 - abs(x_1) ** 3) ** 3)
  } else {
    return(0)
  }
}

make_predictor_matrix = function(x) {
  a = length(x)
  return(cbind(rep(1,a), x))
}
