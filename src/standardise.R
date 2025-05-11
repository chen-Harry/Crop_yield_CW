standardise <- function(x, interval=c(-1, 1)) {
  # standardise x to be within the range of interval via affine transformation
  
  if (any(is.na(x))) {
    stop("argument contains NA")
  }
  
  if (!is.numeric(x)) {
    stop("argument is not numeric")
  }
  
  if (!is.numeric(interval)) {
    stop("interval must be numeric")
  }
  
  min_x <- min(x)
  max_x <- max(x)
  
  if (min_x == max_x) {
    return(rep(0, length(x)))
  }
  
  a <- interval[1]
  b <- interval[2]
  
  return( (b-a) * (x - min_x) / (max_x - min_x) + a)
}