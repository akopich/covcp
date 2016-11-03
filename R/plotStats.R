plotStats = function(statsAndCritValue) {
  stats = statsAndCritValue$statistics$window2statistics
  crit = statsAndCritValue$criticalValue
  
  for (forWindow in stats) {
    plot(forWindow$centralPoints, forWindow$distances, col = ifelse(forWindow$distances > crit,"red", "black"), ylab = "Parameter distance", xlab = "Central point")
    title(paste("Window size = ", forWindow$windowSize))
    abline(h = crit, col = "red")
  }
}