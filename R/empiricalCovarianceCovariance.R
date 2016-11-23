# R package for change-point detection in covariance structure
# Copyright (C) 2016 Valeriy Avanesov acopich@gmail.com 

empiricalCovariance = function(stableSet, hatTheta) Cov(vectorWiseCovariances(stableSet))
