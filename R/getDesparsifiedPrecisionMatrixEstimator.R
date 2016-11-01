getDesparsifiedPrecisionMatrixEstimator = function(Var, GL) function(windowSize) function(data) {
  getDesparsifiedEstimator(data, GL(data)) / sqrt(Var/windowSize)
}

getDesparsifiedEstimator = function(data, theta) {
  sigma = myCov(data)
  theta + t(theta) - t(theta) %*% sigma %*% theta
}