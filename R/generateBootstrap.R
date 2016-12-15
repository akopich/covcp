generateBootstrap = function(data, N, windowSizes, normalize, distances2statistic, parameterDifferenceNorm) {
  bootstrapSample = drawWithReplacement(data, N)
  
  sapply(windowSizes, function(windowSize) {
    distances2statistic(slidingWindowsDifferenceOfMean(normalize(bootstrapSample, windowSize), 
                                                       windowSize, 
                                                       parameterDifferenceNorm))
  })
}

generateBootstrapValues = function(iterations, 
                                   data, 
                                   N, 
                                   windowSizes, 
                                   normalize, 
                                   distances2statistic, 
                                   parameterDifferenceNorm) {
  parRbindShuffle(1:iterations, function(iter) generateBootstrap(data, 
                                                                 N, 
                                                                 windowSizes, 
                                                                 normalize, 
                                                                 distances2statistic, 
                                                                 parameterDifferenceNorm))
}