# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

precisionMatrixStatistic = function(windowSizes, 
                                    data,
                                    distances2statistic, 
                                    windowSize2Data2Parameter, 
                                    diffNorm) {
  window2statistics = lapply(windowSizes, function(windowSize) {
    if (class(windowSize2Data2Parameter) != "function") {
      if (windowSize2Data2Parameter == "mean") {
        distances = slidingWindowsDifferenceOfMean(data / sqrt(windowSize), 
                                                    windowSize, 
                                                    diffNorm) 
      }
    } else {
      distances = slidingWindows(data, 
                                  windowSize, 
                                  windowSize2Data2Parameter(windowSize), 
                                  diffNorm)
    }
    
    distances$statistics = max(distances$distances)
    distances$windowSize = windowSize
    distances    
  })
  
  statistic = maxSapply(window2statistics, function(x) x$statistic)
  
  list("statistics" = statistic, "window2statistics" = window2statistics)
}

createPrecisionMatrixTest = function(windowSizes, 
                                     alpha,
                                     data, 
                                     distances2statistic, 
                                     GL, 
                                     diffNorm, 
                                     stableSetIndexs, 
                                     getVar,
                                     bootstrapIterations = 1000,
                                     nonSymmetricGL = GL) {
  stableSet = data[stableSetIndexs, ]
  
  hatTheta = GL(stableSet)
  Var = getVar(stableSet, hatTheta)
  
  stats = precisionMatrixStatistic(windowSizes, 
                                       data, 
                                       distances2statistic, 
                                       getDesparsifiedPrecisionMatrixEstimator(Var, nonSymmetricGL),
                                       diffNorm) 
  
  criticalValues = precisionMatrixBootstrapBasedCriticalLevel(stableSet, 
                                                             bootstrapIterations, 
                                                             alpha,
                                                             hatTheta, 
                                                             nrow(data), 
                                                             windowSizes, 
                                                             diffNorm, 
                                                             distances2statistic,
                                                             Var) 
  
  list("statistics" = stats, "criticalValue" = criticalValues)
}














