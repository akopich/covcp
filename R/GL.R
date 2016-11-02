# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

chooseRho = function(data) {
  sqrt(log(ncol(data)) / nrow(data))
}

logDetGL = function(thr = 0.01) function(data) { 
  theta = glasso(myCov(data), chooseRho(data), penalize.diagonal = F)$wi
  theta = (theta + t(theta)) / 2
  
  theta[abs(theta) < thr] = 0
  
  theta
}