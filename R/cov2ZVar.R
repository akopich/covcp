cov2ZVar = function(nullCov) function(stableSet, hatTheta) {
  theta = solve(nullCov)
  theta[abs(theta) < 1e-5] = 0
  
  getGaussianSigmas(theta, NULL)
}
