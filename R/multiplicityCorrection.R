checkStats = function(threshold) function(stats) {
  all(stats <= threshold)
}

probabilityToReject = function(bootstrapSamples, threshold) {
  1 - mean(apply(bootstrapSamples, 1, checkStats(threshold)))
}

getThreshold = function(bootstrapSamples, alpha) {
  sorted = apply(bootstrapSamples, 2, function(x) sort(x, decreasing = TRUE))
  
  i = 0
  
  repeat {
    i = i + 1
    
    if (probabilityToReject(bootstrapSamples, sorted[i, ]) > alpha ) {
      break
    }
  }
  
  if (i == 1) {
    result = sorted[1, ]
  } else {
    result = sorted[i - 1, ]
  }
  
  result
}



