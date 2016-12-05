parX = function(x) function(values, f) foreach(val = values, .combine = x) %dopar% {
  f(val)
}

parSapply = parX('c')

parRbind = parX('rbind')

