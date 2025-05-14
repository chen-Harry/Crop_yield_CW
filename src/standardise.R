#' Standardise vector by linearly scaling to within an interval
#'
#' @param x numerical vector, must not contain NAs
#' @param interval vector with 2 values for the end points of the interval
#'
#' @returns vector of same length as x, linearly scaled to be within the interval
#' @export
#'
#' @examples
#' standardise(0:20, c(0, 1))
#' 
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
  
  if (interval[1] > interval[2]) {
    stop("invalid interval")
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