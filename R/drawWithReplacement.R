drawWithReplacement = function(data, N) data[sample(1:nrow(data), N, replace = T), ]

drawWithReplacementWithWeights = function(data, N) sweep(drawWithReplacement(data, N), 1, sign(rnorm(N)), '*') 