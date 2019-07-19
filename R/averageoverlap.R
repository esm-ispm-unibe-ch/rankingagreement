# Average Overlap
averageoverlap <- function(x, y, k) {
  Ad <- sapply(1:k, function(i) length(intersect(x[1:i], y[1:i])))/seq(1,k)
  AO <- cumsum(Ad)/seq(1,k)
  return(AO)
}

