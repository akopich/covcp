# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

slidingWindows = function(data, windowSize, getParameters, parameterDifferenceNorm) {
  parameters = parRbind(1:(nrow(data) - windowSize + 1), function(i) {
    windowData = data[i:(i + windowSize - 1), ]
    getParameters(windowData)
  })
  
  numberOfEstimators = nrow(parameters)
  
  distances = sapply(1:(numberOfEstimators - windowSize), function(i) {
    left = parameters[i, ]
    right = parameters[i + windowSize, ]
    
    parameterDifferenceNorm(left - right)
  })
  
  centralPoints = (windowSize + 1):numberOfEstimators
  
  list("centralPoints" = centralPoints, "distances" = distances)
}