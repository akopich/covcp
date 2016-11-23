# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

empiricalPrecisionMatrixCov = function(stableSet, hatTheta) {
  Zs = getZs(hatTheta, stableSet)
  Cov(Zs)
}