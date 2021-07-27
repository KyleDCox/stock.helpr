#' calculates the derivative of a numeric vector
#'
#' @param SERIES a numeric vector
#'
#' @return numeric

derivative <- function(SERIES){
  total <- length(SERIES)
  d <- rep(NA, total)

  for(i in 2: total){
    d[i] <- SERIES[i] - SERIES[i-1]
  }
  return(d)
}
