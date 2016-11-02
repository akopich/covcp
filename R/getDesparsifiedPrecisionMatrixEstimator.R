# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

getDesparsifiedPrecisionMatrixEstimator = function(Var, GL) function(windowSize) function(data) {
  getDesparsifiedEstimator(data, GL(data)) / sqrt(Var/windowSize)
}

getDesparsifiedEstimator = function(data, theta) {
  sigma = myCov(data)
  theta + t(theta) - t(theta) %*% sigma %*% theta
}