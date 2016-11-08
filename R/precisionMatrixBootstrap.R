# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

getZ = function(theta, x) {
  theta %*% (x %*% t(x)) %*% theta
}

getZs = function(theta, X) {
  t(apply(X, 1, function(x) as.vector(getZ(theta,x))))
}

precisionMatrixBootstrapBasedCriticalLevel = function(stable, 
                                                      iterations, 
                                                      alpha,
                                                      theta, 
                                                      N, 
                                                      windowSizes, 
                                                      parameterDifferenceNorm, 
                                                      distances2statistic,
                                                      Var) {
  means = colMeans(stable)
  stable = sweep(stable, 2, means, '-')
  Zs = getZs(theta, stable)
  
  SD = sqrt(Var)
  
  normalize = function(bootSample, windowSize) sweep(bootSample, 2, as.vector(SD/sqrt(windowSize)), '/')
  
  bootstrappedValues = generateBootstrapValues(iterations, 
                                               Zs, 
                                               N, 
                                               windowSizes, 
                                               normalize, 
                                               distances2statistic, 
                                               parameterDifferenceNorm)
  
  unname(quantile(bootstrappedValues, probs = c(1 - alpha)))
}






