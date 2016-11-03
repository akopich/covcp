maxBy = function(xs, getKey) {
  keys = lapply(xs, getKey)
  xs[[which.max(keys)]]
}