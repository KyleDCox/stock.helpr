#' calculates the exponential moving average of a numeric vector
#'
#' @param DAYS the number of days used to calculate the moving average
#' @param SERIES a numeric vector
#'
#' @return numeric

ema <- function(DAYS, SERIES){
  total <- length(SERIES)
  SERIES_SHORT <- SERIES[1: (total - 1)]
  sma <- moving_average(DAYS, SERIES)[1: (total - 1)]
  weight <- 2 / (DAYS + 1)
  return((SERIES_SHORT - sma) * weight + sma)
}
