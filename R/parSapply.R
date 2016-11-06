parSapply = function(values, f) {
  as.numeric(foreach(val = values, .combine = 'c') %dorng% {
    f(val)
  })
}

parRbind = function(values, f) {
  foreach(val = values, .combine = 'rbind') %dorng% {
    f(val)
  }
}