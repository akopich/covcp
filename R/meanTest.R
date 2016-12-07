getMeanEstimator = function(window) function(data) {
  colSums(data) / sqrt(window)
}

divideColumnWise = function(data, vars) {
  normalizer = 1 / sqrt(vars)
  t(apply(data, 1, function(row) row * normalizer))
}

createMeanTest = function(windowSizes, 
                                     alpha,
                                     data, 
                                     distances2statistic, 
                                     diffNorm, 
                                     stableSetIndexs, 
                                     bootstrapIterations = 1000) {
  stableSet = data[stableSetIndexs, ]
  
  vars = empiricalCovariance(stableSet, hatTheta) 
  data = divideColumnWise(data, vars)
  stableSet = divideColumnWise(stableSet, vars)
  
  stats = precisionMatrixStatistic(windowSizes, 
                                   data, 
                                   distances2statistic, 
                                   getMeanEstimator,
                                   diffNorm) 
  
  criticalValues = meanBootstrapBasedCriticalLevel(stableSet, 
                                          bootstrapIterations, 
                                          alpha,
                                          nrow(data), 
                                          windowSizes, 
                                          diffNorm, 
                                          distances2statistic)
  list("statistics" = stats, "criticalValue" = criticalValues)
}



meanBootstrapBasedCriticalLevel = function(stable, 
                                            iterations, 
                                            alpha,
                                            N, 
                                            windowSizes, 
                                            parameterDifferenceNorm, 
                                            distances2statistic) {
  bootstrappedValues = generateBootstrapValues(iterations, 
                                               stable, 
                                               N, 
                                               windowSizes, 
                                               function(bootstrapSample, windowSize) bootstrapSample / sqrt(windowSize), 
                                               distances2statistic, 
                                               parameterDifferenceNorm) 
  
  getThreshold(bootstrappedValues, alpha)
}









