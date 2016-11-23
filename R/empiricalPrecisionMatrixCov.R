# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

empiricalPrecisionMatrixCov = function(stableSet, hatTheta) {
  p = nrow(hatTheta)
  Zs = getZs(hatTheta, stableSet)
  matrix(diag(cov(Zs)), p, p) 
}