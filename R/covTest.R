covTest = function(windowSizes, 
                    alpha,
                    data, 
                    distances2statistic, 
                    diffNorm, 
                    stableSetIndexs, 
                    scale = TRUE,
                    bootstrapIterations = 1000) {
  data = vectorWiseCovariances(data)
  data = scale(data, center = colMeans(data[stableSetIndexs,]), scale = F)
  
  createMeanTest(windowSizes, 
                 alpha,
                 data, 
                 distances2statistic, 
                 diffNorm, 
                 stableSetIndexs, 
                 scale,
                 bootstrapIterations)
}

vectorWiseCovariances = function(data) t(apply(data, 1, function(x) {
    cov = x %*% t(x)
    as.vector(cov[upper.tri(cov, diag=TRUE)])
  } ))
   