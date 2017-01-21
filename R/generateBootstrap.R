generateBootstrap = function(data, N, windowSizes, normalize, distances2statistic, parameterDifferenceNorm) {
  bootstrapSample = drawWithReplacementWithWeights(data, N)
  
  sapply(windowSizes, function(windowSize) {
     (distances2statistic %.%
     slidingWindowsDifferenceOfMean %2% windowSize %2% parameterDifferenceNorm %.%
     normalize %2% windowSize) (bootstrapSample)
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