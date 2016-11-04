plotStats = function(statsAndCritValue) {
  stats = statsAndCritValue$statistics$window2statistics
  crit = statsAndCritValue$criticalValue
  
  for (forWindow in stats) {
    plot(forWindow$centralPoints, 
         forWindow$distances, 
         col = ifelse(forWindow$distances > crit,"red", "black"), 
         xlab = "Central point",
         ylab = "Parameter distance", 
         ylim = c(0, max(crit, max(forWindow$distances))))
    title(paste("Window size = ", forWindow$windowSize))
    abline(h = crit, col = "red")
  }
}