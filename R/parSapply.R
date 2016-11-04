parSapply = function(values, f) {
  as.numeric(foreach(val = values, .combine = 'c') %dorng% {
    f(val)
  })
}