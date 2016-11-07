getMeanEstimator = function(window) function(data) {
  colSums(data) / sqrt(window)
}

createMeanTest = function(windowSizes, 
                                     alpha,
                                     data, 
                                     distances2statistic, 
                                     diffNorm, 
                                     stableSetIndexs, 
                                     bootstrapIterations = 1000) {
  stableSet = data[stableSetIndexs, ]
  
  stats = precisionMatrixStatistic(windowSizes, 
                                   data, 
                                   distances2statistic, 
                                   getMeanEstimator,
                                   diffNorm) 
  
  criticalValue = meanBootstrapBasedCriticalLevel(stableSet, 
                                          bootstrapIterations, 
                                          alpha,
                                          nrow(data), 
                                          windowSizes, 
                                          diffNorm, 
                                          distances2statistic)
  list("statistics" = stats, "criticalValue" = criticalValue)
}



meanBootstrapBasedCriticalLevel = function(stable, 
                                            iterations, 
                                            alpha,
                                            N, 
                                            windowSizes, 
                                            parameterDifferenceNorm, 
                                            distances2statistic) {
  bootstrappedValues = parSapply(1:iterations, function(iter) {
    bootstrapSample = drawWithReplacement(stable, N)
    
    maxSapply(windowSizes, function(windowSize) {
      distances2statistic(slidingWindowsDifferenceOfMean(bootstrapSample * sqrt(windowSize), 
                                                         windowSize, 
                                                         parameterDifferenceNorm))
    })
  })
  
  unname(quantile(bootstrappedValues, probs = c(1 - alpha)))
}









