slidingWindows = function(data, windowSize, getParameters, parameterDistance) {
  parameters = lapply(1:(nrow(data) - windowSize + 1), function(i) {
    windowData = data[i:(i + windowSize - 1), ]
    getParameters(windowData)
  })
  
  distances = sapply(1:(length(parameters) - windowSize), function(i) {
    left = parameters[[i]]
    right = parameters[[i + windowSize]]
    
    parameterDistance(left, right)
  })
  
  distances
}