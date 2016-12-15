parRbindShuffle = function(values, f) {
  numberOfChunks = detectCores() * 4
  
  chunkIndx = rep(1:numberOfChunks, length = length(values))
  chunks = split(values, chunkIndx)
  
  foreach(chunk = chunks, .combine = 'rbind', .inorder = FALSE) %dopar% {
    Reduce('rbind', lapply(chunk, f))
  }
}

parRbind = function(values, f) {
  foreach(val = values, .combine = 'rbind') %dopar% {
    f(val)
  }
}