# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

precisionMatrixStatistic = function(windowSizes, 
                                    data,
                                    distances2statistic, 
                                    windowSize2Data2Parameter, 
                                    diffNorm) {
  max(sapply(windowSizes, function(windowSize) {
    distances2statistic(slidingWindows(data, 
                                       windowSize, 
                                       windowSize2Data2Parameter(windowSize), 
                                       diffNorm))
  }))
}

createPrecisionMatrixTest = function(windowSizes, 
                                     data, 
                                     distances2statistic, 
                                     GL, 
                                     diffNorm, 
                                     stableSetIndexs, 
                                     getVar) {
  stableSet = data[stableSetIndexs, ]
  
  hatTheta = GL(stableSet)
  
  Var = getVar(stableSet, hatTheta)
  
  statistic = precisionMatrixStatistic(windowSizes, 
                                       data, 
                                       distances2statistic, 
                                       getDesparsifiedPrecisionMatrixEstimator(Var, GL),
                                       diffNorm) 
  
  statistic
}














