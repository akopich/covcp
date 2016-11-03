# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

cov2ZVar = function(nullCov) function(stableSet, hatTheta) {
  theta = solve(nullCov)
  theta[abs(theta) < 1e-5] = 0
  
  getGaussianSigmas(theta, NULL)
}

getGaussianSigmas = function(theta, data) {
  d = diag(theta)
  return(theta^2 + d %*% t(d))
}