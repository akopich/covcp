covTest = function(windowSizes, 
                    alpha,
                    data, 
                    distances2statistic, 
                    diffNorm, 
                    stableSetIndexs, 
                    bootstrapIterations = 1000) {
  data = vectorWiseCovariances(data)
  data = scale(data, scale = F)
  
  createMeanTest(windowSizes, 
                 alpha,
                 data, 
                 distances2statistic, 
                 diffNorm, 
                 stableSetIndexs, 
                 bootstrapIterations)
}

vectorWiseCovariances = function(data) t(apply(data, 1, function(x) {
    cov = x %*% t(x)
    as.vector(cov[upper.tri(cov, diag=TRUE)])
  } ))
   