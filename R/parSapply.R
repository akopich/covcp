parX = function(x) function(values, f) {
  numberOfChunks = detectCores() * 4
  
  chunkIndx = rep(1:numberOfChunks, length = length(values))
  chunks = split(values, chunkIndx)
  
  foreach(chunk = chunks, .combine = x) %dopar% {
    Reduce(x, lapply(chunk, f))
  }
}

parSapply = parX('c')

parRbind = parX('rbind')

