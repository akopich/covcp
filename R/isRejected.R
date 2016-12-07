isRejected = function(stats) {
  values = sapply(stats$statistics$window2statistics, function(x) x$statistics)
  threshold = stats$criticalValue
  
  !checkStats(threshold)(values)
}