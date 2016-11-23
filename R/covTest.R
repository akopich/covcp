covTest = function(windowSizes, 
                    alpha,
                    data, 
                    distances2statistic, 
                    diffNorm, 
                    stableSetIndexs, 
                    bootstrapIterations = 1000) {
  data = vectorWiseCovariances(data)
  
  createMeanTest(windowSizes, 
                 alpha,
                 data, 
                 distances2statistic, 
                 diffNorm, 
                 stableSetIndexs, 
                 bootstrapIterations)
}

vectorWiseCovariances = function(data) t(apply(data, 1, function(x) as.vector(x %*% t(x))))
   