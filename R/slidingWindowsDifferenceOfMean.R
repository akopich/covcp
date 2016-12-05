# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

slidingWindowsDifferenceOfMean = function(data, windowSize, parameterDifferenceNorm) {
  i = 1
  left  = colSums(data[i:(i + windowSize - 1), ])
  right = colSums(data[(i + windowSize):(i + 2*windowSize - 1), ])
  difference = left - right
  
  distances = c()
  N = nrow(data)
  
  repeat {
    distances = c(distances, parameterDifferenceNorm(difference))
    
    if (i == N - 2 * windowSize + 1) {
      break
    }
    
    i = i + 1
    
    difference = difference - data[i - 1,] + 2 * data[i + windowSize - 1, ] - data[i + 2*windowSize - 1, ]
  }
  
  centralPoints = (windowSize + 1):(N - windowSize + 1)
  
  list("centralPoints" = centralPoints, "distances" = distances)
}