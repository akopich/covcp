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
  
  bootstrappedValues = parSapply(1:iterations, function(iter) {
    bootstrapZ = Zs[sample(1:nrow(stable), N, replace = T), ]
    maxSapply(windowSizes, function(windowSize) {
      normalizedZ = sweep(bootstrapZ, 2, as.vector(SD/sqrt(windowSize)), '/')
      distances2statistic(slidingWindowsDifferenceOfMean(normalizedZ, 
                                                        windowSize, 
                                                        parameterDifferenceNorm))
    })
  })
  
  unname(quantile(bootstrappedValues, probs = c(1-alpha)))
}



parSapply = function(values, f) {
  as.numeric(foreach(val = values, .combine = 'c') %dorng% {
    f(val)
  })
}



