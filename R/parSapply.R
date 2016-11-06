parX = function(x) function(values, f) foreach(val = values, .combine = x) %dorng% {
  f(val)
}

parSapply = parX('c')

parRbind = parX('rbind')

