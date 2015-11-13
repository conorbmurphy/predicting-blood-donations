un <- function(x, k) {
  n <- nrow(Training.Data)
  k2 <- k/2
  pred <- vector(length=n-k)
  for (i in 1:(n-k)) {
    if(sum(x[i:(i+k-1)])) >= k2) pred[i] <- 1 else pred[i] <- 0
    }
  return(mean(abs(pred-x[(k+1):n])))
}
